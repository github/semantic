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
