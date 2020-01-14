case class Project(name: String)
def (name: String) dependencies = ???
def (project: Project) dependencies = project.name.dependencies // error, following a cyclic reference warning