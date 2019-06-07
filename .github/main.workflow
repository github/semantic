workflow "Build and publish to GitHub Package Registry" {
  on = "push"
  resolves = [
    "Docker Registry",
    "Docker Build",
    "GitHub Action for Docker",
  ]
}

action "Docker Registry" {
  uses = "actions/docker/login@8cdf801b322af5f369e00d85e9cf3a7122f49108"
  secrets = ["DOCKER_PASSWORD", "DOCKER_USERNAME", "DOCKER_REGISTRY_URL"]
  needs = ["Docker Build"]
}

action "Docker Build" {
  uses = "actions/docker/cli@8cdf801b322af5f369e00d85e9cf3a7122f49108"
  args = "build -t github/semantic ."
}

action "Docker Tag" {
  uses = "actions/docker/tag@8cdf801b322af5f369e00d85e9cf3a7122f49108"
  needs = ["Docker Registry"]
  args = "github/semantic docker.pkg.github.com/github/semantic"
}

action "GitHub Action for Docker" {
  uses = "actions/docker/cli@8cdf801b322af5f369e00d85e9cf3a7122f49108"
  needs = ["Docker Tag"]
  args = "docker push docker.pkg.github.com/github/semantic"
}
