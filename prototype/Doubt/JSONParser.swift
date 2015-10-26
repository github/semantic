//
//  JSONParser.swift
//  Doubt
//
//  Created by Josh Vera on 10/16/15.
//  Copyright © 2015 GitHub. All rights reserved.
//

import Foundation
import Madness
import Either
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

// Quoted strings parser
// TODO: Improve string parsing
let stringBody: StringParser = { $0.map({ String($0) }).joinWithSeparator("") } <^>
		String.lift(noneOf("\"")*)
let quoted = %"\"" *> stringBody <* %"\"" <* whitespace

typealias MembersParser = Parser<String, [(String, CofreeJSON)]>.Function;

// Parses an array of (String, CofreeJSON) object members
func members(json: JSONParser) -> MembersParser {
	let pairs: Parser<String, (String, CofreeJSON)>.Function = (curry(pair) <^>
		quoted
		<* whitespace
		<* %":"
		<* whitespace
		<*> json)

	let separatedPairs: MembersParser  = (%"," *> whitespace *> pairs <* whitespace)*

	let oneOrMore: MembersParser = curry { [$0] + $1 } <^>
		pairs
		<* whitespace
		<*> separatedPairs

	return oneOrMore <|> pure([])
}

typealias ValuesParser = Parser<String, [CofreeJSON]>.Function;

// Parses an array of CofreeJSON array values
func elements(json: JSONParser) -> ValuesParser {
	let value: Parser<String, CofreeJSON>.Function = whitespace *> json <* whitespace

	return sepBy(value, %",")
}

public let json: JSONParser = fix { json in
	// TODO: Parse backslashed escape characters

	let string: JSONParser = quoted --> {
		Cofree($1, .Leaf(.String($2)))
	} <?> "string"

	let array: JSONParser =  %"["
		<* whitespace
		*> elements(json)
		<* %"]"
		<* whitespace --> {
			Cofree($1, .Indexed($2))
		} <?> "array"

	let object: JSONParser = %"{"
		*> whitespace
		*> members(json)
		<* whitespace
		<* %"}"
		<* whitespace
		--> { (_, range, values) in
			let vs: [CofreeJSON] = values.map({ (string, value) in
				let valueRange = value.extract
				let newRange = Range(start: range.startIndex, end: valueRange.endIndex)
				return Cofree(newRange, .Keyed([string: value]))
			})

			return Cofree(range, .Fixed(vs))
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
	return object <|> array <|> string <|> numberParser <|> boolean <|> null
}
