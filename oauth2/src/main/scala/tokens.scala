package unfiltered.oauth2

trait Token {
  def value: String
  def refresh: Option[String]
  def expiresIn: Option[Int]
  def clientId: String
  def redirectUri: String
  def scopes: Option[String]
  def owner: String
  def tokenType: String
}

trait TokenStore {
  def refresh(t: Token): Token
  def token(code: String): Option[Token]
  def clientToken(clientId: String): Option[Token]
  def accessToken(code: String): Option[Token]
  def generateAccessToken(other: Token): Token
  /** @return a short lived Token bound to a client and redirect uri for a given resource owner. */
  def generateCodeToken(owner: ResourceOwner, client: Client, scope: Option[String], redirectUri: String): String
  def generateImplicitAccessToken(owner: ResourceOwner, client: Client, scope: Option[String], redirectUri: String): Token
}