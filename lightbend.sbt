resolvers in ThisBuild += "lightbend-commercial-mvn" at
  "https://repo.lightbend.com/pass/Umak-6tdaOdMVRkFJ1DFRi9Fp9OPNvrM_Ex0bpi0OcGWmYi-/commercial-releases"
resolvers in ThisBuild += Resolver.url("lightbend-commercial-ivy",
  url("https://repo.lightbend.com/pass/Umak-6tdaOdMVRkFJ1DFRi9Fp9OPNvrM_Ex0bpi0OcGWmYi-/commercial-releases"))(Resolver.ivyStylePatterns)