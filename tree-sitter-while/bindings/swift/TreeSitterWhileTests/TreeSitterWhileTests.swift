import XCTest
import SwiftTreeSitter
import TreeSitterWhile

final class TreeSitterWhileTests: XCTestCase {
    func testCanLoadGrammar() throws {
        let parser = Parser()
        let language = Language(language: tree_sitter_while())
        XCTAssertNoThrow(try parser.setLanguage(language),
                         "Error loading While grammar")
    }
}
