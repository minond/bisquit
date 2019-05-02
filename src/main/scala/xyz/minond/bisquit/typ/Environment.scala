package xyz.minond.bisquit.typ

case class Environment(uni: Map[String, Ty]) {
  def get(name: String) =
    uni.get(name)
}

object Environment {
  def create(): Environment =
    Environment(Map())
}
