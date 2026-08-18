package main

import "core:strconv"
import "core:bufio"
import "core:fmt"
import "core:os"

/* Intended Grammar:
 *  S ::= expression
 *      | ''
 *
 *  expression ::= addition
 *
 *  addition ::= multiplication (('+'|'-') multiplication)*
 *
 *  multiplication ::= unary (('*'|'/') unary)*
 *
 *  unary ::= ('+'|'-')* atom
 *
 *  atom ::= '(' expression ')'
 *         | number
 *
 */

Operator :: enum byte {
	None,
	Negation,
	Multiplication,
	Division,
	Addition,
	Subtraction,
}

Node :: struct {
	operator: Operator,
	left:     ^Node,
	right:    ^Node,
	number:   i64,
}

Token_Type :: enum byte {
	None,
	Number,
	Plus,
	Minus,
	Star,
	Slash,
	L_Paren,
	R_Paren,
}

Token :: struct {
	type:   Token_Type,
	number: i64,
}

// n.b. uses context.temp_allocator
// Caller must handle empty tokens.
parse :: proc(tokens: []Token) -> (ast: ^Node, ok: bool) {
	ts := Token_Stream{tokens=tokens}
	ast = parse_addition(&ts) or_return
	if current(&ts) != {} {return nil, false}
	return
	// NOTE: Odin errors on `if current(&ts) != Token{}` saying
	// the rhs is not an expression, but a type. Compiler error?

	parse_addition :: proc(ts: ^Token_Stream) -> (ast: ^Node, ok: bool) {
		ast = parse_multiplication(ts) or_return
		for {
			tok := current(ts).type
			if tok != .Plus && tok != .Minus {break}

			rhs := parse_multiplication(ts) or_return
			lhs := ast
			ast = new_clone(Node{
				operator = binop_from_token(tok),
				left = lhs,
				right = rhs,
			}, context.temp_allocator)
		}
		return ast, true
	}
	parse_multiplication :: proc(ts: ^Token_Stream) -> (ast: ^Node, ok: bool) {
		ast = parse_unary(ts) or_return
		for {
			tok := current(ts).type
			if tok != .Star && tok != .Slash {break}

			rhs := parse_unary(ts) or_return
			lhs := ast
			ast = new_clone(Node{
				operator = binop_from_token(tok),
				left = lhs,
				right = rhs,
			}, context.temp_allocator)
		}
		return ast, true
	}
	parse_unary :: proc(ts: ^Token_Stream) -> (ast: ^Node, ok: bool) {
		minus_cnt: u32 = 0
		// NOTE:
		// The plan is to emit at max one negation node:
		// the number of `-` is tracked; if it is even, then
		// it is a no-op and only the atom node is emited;
		// if, however, it is odd, then the atom node is wrapped
		// by exactly one negation node. TODO number optimization
		// Also, `+` are no-ops.
		for {
			#partial switch op := current(ts); op.type {
			case:        break
			case .Plus:  ;
			case .Minus: minus_cnt += 1
			}
			advance(ts)
		}
		is_negative := minus_cnt % 2 != 0

		value: Node
		#partial switch tok := current(ts); tok.type {
		case:
			ok = false
			value = {}
		case .Number:
			ok = true
			number := -tok.number if is_negative else tok.number
			value = {
				operator = .None,
				number = number
			}
		// TODO: no parentheses for now
		}
		ast = new_clone(value, context.temp_allocator)
		return
	}

	binop_from_token :: proc(t: Token_Type) -> Operator {
		@(static,rodata) table := #partial [Token_Type]Operator {
			.Star = .Multiplication,
			.Slash = .Division,
			.Plus = .Addition,
			.Minus = .Subtraction,
		}
		return table[t]
	}

	Token_Stream :: struct {
		tokens: []Token,
		idx:    int,
	}
	current :: proc(ts: ^Token_Stream) -> Token {
		return ts.tokens[ts.idx] if ts.idx < len(ts.tokens) else Token{}
	}
	advance :: proc(ts: ^Token_Stream) {
		ts.idx = min(ts.idx + 1, len(ts.tokens))
	}
}

// n.b. uses context.temp_allocator.
lex :: proc(line: string) -> (tokens: [dynamic]Token, ok: bool) {
	line := transmute([]u8)line // Avoid utf-8 decoder cost.
	tokens = make([dynamic]Token, context.temp_allocator)
	digits := make([dynamic]u8, context.temp_allocator)
	state: State = .Free
	for c in line {
		switch c {
		case ' ', '\t', '\n':
			switch state {
			case .In_Number:
				emit_number_token(digits[:], &tokens) or_return
				clear(&digits)
			case .Free:
				;
			}
			state = .Free
		case '+', '-', '*', '/', '(', ')':
			switch state {
			case .In_Number:
				emit_number_token(digits[:], &tokens) or_return
				clear(&digits)
				fallthrough
			case .Free:
				t := from_operator_to_token(c)
				append(&tokens, t)
			}
			state = .Free
		case '0'..='9':
			switch state {
			case .In_Number, .Free:
				append(&digits, c)
			}
			state = .In_Number
		case:
			return
		}
	}
	if len(digits) > 0 {
		emit_number_token(digits[:], &tokens) or_return
	}
	ok = true
	return


	State :: enum byte {In_Number, Free}

	emit_number_token :: proc(digits: []byte, tokens: ^[dynamic]Token) -> bool {
		digits := cast(string)digits[:]
		n := strconv.parse_i64(digits) or_return
		// FIXME: The stdlib parser is buggy, and does not error on big inputs.
		token := Token{number = n, type = .Number}
		append(tokens, token)
		return true
	}

	from_operator_to_token :: proc(ch: byte) -> Token {
		// A small look-up table is used.
		// The interesting values are between ASCII 40 and 47.
		// There are holes, but those will be mapped to `None`.
		lower_bound :: 40
		upper_bound :: 47
		n_entries :: upper_bound - lower_bound + 1
		@(static,rodata) table := [n_entries]Token_Type {
			'+' - lower_bound = .Plus,
			'-' - lower_bound = .Minus,
			'*' - lower_bound = .Star,
			'/' - lower_bound = .Slash,
			'(' - lower_bound = .L_Paren,
			')' - lower_bound = .R_Paren,
		}
		idx := ch - lower_bound // Wraps if ch < lower_bound.
		type := table[idx]  if idx < n_entries  else .None
		return {type = type}
	}
}

compute :: proc(node: ^Node) -> i64 {
	if node == nil {return 0}

	lhs := compute(node.left)
	rhs := compute(node.right)
	value: i64
	switch node.operator {
	case .None:
		value = node.number
	case .Negation:
		value = -lhs
	case .Multiplication:
		value = lhs * rhs
	case .Division:
		value = lhs / rhs
	case .Addition:
		value = lhs + rhs
	case .Subtraction:
		value = lhs - rhs
	}
	return value
}

main :: proc() {
	r: bufio.Reader
	bufio.reader_init(&r, os.to_reader(os.stdin))
	defer bufio.reader_destroy(&r)
	for {
		defer free_all(context.temp_allocator)
		fmt.print("> ")
		line := bufio.reader_read_string(&r, '\n') or_break
		tokens := lex(line) or_break
		if len(tokens) == 0 {continue}
		ast := parse(tokens[:]) or_break
		result := compute(ast)
		fmt.println(result)
	}
}
