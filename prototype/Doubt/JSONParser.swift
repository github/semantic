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

let stringBody: StringParser = { $0.map({ String($0) }).joinWithSeparator("") } <^>
		not(%"\\" <|> %"\"")*
let quoted = %"\"" *> stringBody <* %"\""
let whitespace: CharacterParser  = satisfy({ (c: Character) in c == " " })*

typealias MembersParser = Parser<String, [(String, CofreeJSON)]>.Function;

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
	let array: JSONParser =  %"[" *> elements(json) <* %"]" --> { Cofree($1, .Indexed($2)) }

	let object: JSONParser =
		%"{"
			*> whitespace
			*> members(json)
			<* whitespace
			<* %"}"
		--> { (_, range, values) in
			Cofree(range, .Keyed(Dictionary(elements: values)))
		}

	// TODO: Parse Numbers correctly
	let number: JSONParser = %"0" --> { Cofree($1, .Leaf(.String($2))) }

	// TODO: This should just be dict <|> array and Value = dict | array | string | number | null | bool
	return object <|> array <|> string <|> number
}