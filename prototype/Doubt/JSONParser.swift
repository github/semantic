//
//  JSONParser.swift
//  Doubt
//
//  Created by Josh Vera on 10/16/15.
//  Copyright © 2015 GitHub. All rights reserved.
//

import Foundation
import Madness
import Prelude

public typealias CofreeJSON = Cofree<JSONLeaf, Range<String.CharacterView.Index>>
public typealias JSONParser = Parser<String.CharacterView, CofreeJSON>.Function

typealias StringParser = Parser<String.CharacterView, String>.Function
typealias CharacterParser = Parser<String.CharacterView, [Character]>.Function

// Inlined for performance reasons
let whitespace: CharacterParser = oneOf(" \n\r\t")*

// TODO: Parse unicode escape sequence
let escapeChar: StringParser = curry(+) <^> %"\\" <*> ({ String($0) } <^> oneOf("\\\"bfnrt"))
let otherChar: StringParser = { String($0) } <^> noneOf("\"\\")

// Quoted strings parser
// TODO: Improve string parsing
let stringBody: StringParser = { $0.joinWithSeparator("") } <^> many(escapeChar <|> otherChar)
let quoted = %"\"" *> stringBody <* %"\""

typealias MembersParser = Parser<String.CharacterView, [(String, CofreeJSON)]>.Function;

// Parses an array of (String, CofreeJSON) object members
func members(json: JSONParser) -> MembersParser {
	let keyAndKeyTerm: Parser<String.CharacterView, (String, CofreeJSON)>.Function = quoted --> { (_, range, key) in
		(key, Cofree(range, .Leaf(.String(key))))
	}
	let pairs: Parser<String.CharacterView, (String, CofreeJSON)>.Function = (curry(pair) <^>
		keyAndKeyTerm
		<* whitespace
		<* %":"
		<* whitespace
		<*> json) --> { (_, range, values) in
			let key = values.0.0
			return (key, Cofree(range, .Fixed([values.0.1, values.1])))
		}

	return sepBy(pairs, whitespace <* %"," <* whitespace)
}

typealias ValuesParser = Parser<String.CharacterView, [CofreeJSON]>.Function;

// Parses an array of CofreeJSON array values
func elements(json: JSONParser) -> ValuesParser {
	let value: Parser<String.CharacterView, CofreeJSON>.Function = whitespace *> json
	return sepBy(value, whitespace <* %"," <* whitespace)
}

public let json: JSONParser = fix { json in
	// TODO: Parse backslashed escape characters

	let string: JSONParser = quoted --> {
		Cofree($1, .Leaf(.String($2)))
	} <?> "string"

	let array: JSONParser =  %"["
		<* whitespace
		*> elements(json)
		<* whitespace
		<* %"]"
		--> {
			Cofree($1, .Indexed($2))
		} <?> "array"

	let object: JSONParser = %"{"
		*> whitespace
		*> members(json)
		<* whitespace
		<* %"}"
		--> { (_, range, values: [(String, CofreeJSON)]) in
			Cofree(range, .Keyed(Dictionary(elements: values)))
		} <?> "object"

	let doubleParser: DoubleParser = number
	let numberParser: JSONParser = (doubleParser --> { _, range, value in
		let num = JSONLeaf.Number(value)
		return Cofree(range, .Leaf(num))
	}) <?> "number"
	
	let null: JSONParser = %"null" --> { (_, range, value) in
		return Cofree(range, .Leaf(.Null))
	} <?> "null"
	
	let boolean: JSONParser = %"false" <|> %"true" --> { (_, range, value) in
		let boolean = value == "true"
		return Cofree(range, .Leaf(.Boolean(boolean)))
	} <?> "boolean"

	// TODO: This should be JSON = dict <|> array and
	// Value = dict | array | string | number | null | bool
	return (object <|> array <|> string <|> numberParser <|> boolean <|> null) <* whitespace
}
