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
import Doubt

typealias CofreeJSON = Cofree<JSONLeaf, Range<String.CharacterView.Index>>
typealias JSONParser = Parser<String, CofreeJSON>.Function

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

func not<C: CollectionType, T>(parser: Parser<C, T>.Function)(_ input: C, _ index: C.Index) -> Either<Error<C.Index>, (C.Generator.Element, C.Index)> {
	if index.distanceTo(input.endIndex) <= 0 || parser(input, index).right != nil {
		return .Left(Error(reason: "", index: index, children: []))
	} else {
		return .Right(input[index], index.successor())
	}
}


typealias StringParser = Parser<String, String>.Function
typealias CharacterParser = Parser<String, [Character]>.Function

// Inlined for performance reasons
let whitespaceChars: [Character] = [" ", "\n", "\t", "\r"]
let whitespace: CharacterParser = String.lift(satisfy({ whitespaceChars.contains($0) })*)

// Quoted strings parser
let stringBody: StringParser = { $0.map({ String($0) }).joinWithSeparator("") } <^>
		not(%"\\" <|> %"\"")*
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

	let separatedValues: ValuesParser = (%"," *> value)*

	let oneOrMore: ValuesParser = curry { [$0] + $1 } <^>
		value
		<*> separatedValues
	return oneOrMore <|> pure([])
}

let json: JSONParser = fix { json in
	// TODO: Parse backslashed escape characters

	let string: JSONParser = quoted --> { Cofree($1, .Leaf(.String($2))) }
	let array: JSONParser =  %"[" <* whitespace *> elements(json) <* %"]" <* whitespace --> { Cofree($1, .Indexed($2)) }

	let object: JSONParser =
		%"{"
			*> whitespace
			*> members(json)
			<* whitespace
			<* %"}"
			<* whitespace
		--> { (_, range, values) in
			Cofree(range, .Keyed(Dictionary(elements: values)))
		}

	let doubleParser = String.lift(double) <* whitespace
	let number: JSONParser = doubleParser --> { _, range, value in
		let num = JSONLeaf.Number(value)
		return Cofree(range, .Leaf(num))
	}
	
	let null: JSONParser = %"null" --> { (_, range, value) in
		return Cofree(range, .Leaf(.Null))
	}
	
	let boolean: JSONParser = %"false" <|> %"true" --> { (_, range, value) in
		let boolean = value == "true"
		return Cofree(range, .Leaf(.Boolean(boolean)))
	}

	// TODO: This should be JSON = dict <|> array and
	// Value = dict | array | string | number | null | bool
	return object <|> array <|> string <|> number <|> boolean <|> null
}