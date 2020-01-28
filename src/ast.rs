use crate::tokens::Token;

#[derive(Debug, Eq)]
pub struct ASTNode {
    pub token: Token,
    pub children: Vec<ASTNode>
}

impl ASTNode {
    pub fn new(token: Token, children: Vec<ASTNode>) -> ASTNode {
        ASTNode { token, children }
    }
}

impl PartialEq for ASTNode {
    fn eq(&self, other: &ASTNode) -> bool {
        self.token == other.token && self.children == other.children
    }
}

#[test]
fn test_astnode_equality() {
    let n1 = ASTNode::new(
        Token::LiteralStr("foo".to_string()),
        vec![]
    );
    let n2 = ASTNode::new(
        Token::LiteralStr("foo".to_string()),
        vec![]
    );
    let n3 = ASTNode::new(
        Token::LiteralStr("bar".to_string()),
        vec![]
    );

    assert_eq!(n1, n2);
    assert_ne!(n1, n3);
}