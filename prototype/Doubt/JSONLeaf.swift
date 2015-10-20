//
//  JSONLeaf.swift
//  Doubt
//
//  Created by Josh Vera on 10/16/15.
//  Copyright © 2015 GitHub. All rights reserved.
//

import Foundation
import Doubt

typealias Term = Cofree<JSONLeaf, Range<String.Index>>
typealias Diff = Free<JSONLeaf, Patch<Term>>

enum JSONLeaf: CustomJSONConvertible, CustomStringConvertible, Equatable {
	case Number(Double)
	case Boolean(Bool)
	case String(Swift.String)
	case Null


	// MARK: CustomJSONConvertible

	var JSON: Doubt.JSON {
		switch self {
		case let .Number(n):
			return .Number(n)
		case let .Boolean(b):
			return .Boolean(b)
		case let .String(s):
			return .String(s)
		case .Null:
			return .Null
		}
	}


	// MARK: CustomStringConvertible

	var description: Swift.String {
		switch self {
		case let .Number(n):
			return Swift.String(n)
		case let .Boolean(b):
			return Swift.String(b)
		case let .String(s):
			return Swift.String(reflecting: s)
		case .Null:
			return "null"
		}
	}
}

func == (left: JSONLeaf, right: JSONLeaf) -> Bool {
	switch (left, right) {
	case let (.Number(a), .Number(b)):
		return a == b
	case let (.Boolean(a), .Boolean(b)):
		return a == b
	case let (.String(a), .String(b)):
		return a == b
	case (.Null, .Null):
		return true
	default:
		return false
	}
}
//
//extension JSON {
//	init?(path: Swift.String) {
//		guard let data = try? NSData(contentsOfFile: path, options: []) else { return nil }
//		guard let object = try? NSJSONSerialization.JSONObjectWithData(data, options: .AllowFragments) else { return nil }
//		self.init(object: object)
//	}
//
//	var term: Term {
//		func annotate(json: Syntax<Term, JSONLeaf>) -> Term {
//			return Cofree(size(json), json)
//		}
//		func size(syntax: Syntax<Term, JSONLeaf>) -> Int {
//			switch syntax {
//			case .Leaf:
//				return 1
//			case let .Indexed(i):
//				return 1 + i.map { size($0.unwrap) }.reduce(0, combine: +)
//			case let .Keyed(i):
//				return 1 + i.values.map { size($0.unwrap) }.reduce(0, combine: +)
//			}
//		}
//
//		switch self {
//		case let .Array(a):
//			return annotate(.Indexed(a.map { $0.term }))
//		case let .Dictionary(d):
//			return annotate(.Keyed(Swift.Dictionary(elements: d.map { ($0, $1.term) })))
//		case let .Number(n):
//			return annotate(.Leaf(.Number(n)))
//		case let .Boolean(b):
//			return annotate(.Leaf(.Boolean(b)))
//		case let .String(s):
//			return annotate(.Leaf(.String(s)))
//		case .Null:
//			return annotate(.Leaf(.Null))
//		}
//	}
//}
