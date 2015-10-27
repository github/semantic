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
public typealias JSONParser = Parser<String, CofreeJSON>.Function

extension String: CollectionType {
	public var count: Index.Distance {
		return characters.count
	}

	public static func lift<A>(parser: Parser<String.CharacterView, A>.Function) -> Parser<String, A>.Function {
		return {
			parser($0.characters, $1)
		}
	}
}

typealias StringParser = Parser<String, String>.Function
typealias CharacterParser = Parser<String, [Character]>.Function

// Inlined for performance reasons
let whitespaceChars: [Character] = [" ", "\n", "\t", "\r"]
let whitespace: CharacterParser = String.lift(satisfy({ whitespaceChars.contains($0) })*)

// TODO: Parse unicode escape sequence
let escapeChar: StringParser = %"\\\\" <|> %"\\\"" <|> %"\\b" <|> %"\\f" <|> %"\\n" <|> %"\\r" <|> %"\\t"
let otherChar: StringParser = { String($0) } <^> satisfy { c in
	c != "\"" && c != "\\"
}
let charP: StringParser = escapeChar <|> otherChar

// Quoted strings parser
// TODO: Improve string parsing
let stringBody: StringParser = { $0.joinWithSeparator("") } <^> many(charP)
let quoted = %"\"" *> stringBody <* %"\""

typealias MembersParser = Parser<String, [(String, CofreeJSON)]>.Function;

// Parses an array of (String, CofreeJSON) object members
func members(json: JSONParser) -> MembersParser {
	let pairs: Parser<String, (String, CofreeJSON)>.Function = (curry(pair) <^>
		(quoted --> { (_, range, key) -> (String, CofreeJSON) in
			return (key, Cofree(range, .Leaf(.String(key))))
		})
		<* whitespace
		<* %":"
		<* whitespace
		<*> json) --> { (_, range, values) in
			let key = values.0.0
			return (key, Cofree(range, .Fixed([values.0.1, values.1])))
		}

	return sepBy(pairs, whitespace <* %"," <* whitespace)
}

typealias ValuesParser = Parser<String, [CofreeJSON]>.Function;

// Parses an array of CofreeJSON array values
func elements(json: JSONParser) -> ValuesParser {
	let value: Parser<String, CofreeJSON>.Function = whitespace *> json
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
	let numberParser: JSONParser = String.lift(doubleParser --> { _, range, value in
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
