package tree_sitter_while_test

import (
	"testing"

	tree_sitter "github.com/tree-sitter/go-tree-sitter"
	tree_sitter_while "github.com/tree-sitter/tree-sitter-while/bindings/go"
)

func TestCanLoadGrammar(t *testing.T) {
	language := tree_sitter.NewLanguage(tree_sitter_while.Language())
	if language == nil {
		t.Errorf("Error loading While grammar")
	}
}
