/* Generated By:JavaCC: Do not edit this line. CanonicalTermParserVisitor.java Version 5.0 */
package org.cs3.prolog.connector.internal.cterm.parser;

public interface CanonicalTermParserVisitor
{
  public Object visit(SimpleNode node, Object data);
  public Object visit(ASTAtom node, Object data);
  public Object visit(ASTNil node, Object data);
  public Object visit(ASTVariable node, Object data);
  public Object visit(ASTCompound node, Object data);
  public Object visit(ASTInteger node, Object data);
  public Object visit(ASTFloat node, Object data);
  public Object visit(ASTString node, Object data);
}
/* JavaCC - OriginalChecksum=8d10baaf587cef88ec596e158070e26e (do not edit this line) */
