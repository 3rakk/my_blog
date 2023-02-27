library(targets)
"posts/post-with-code/_targets.R"

# Target for rendering the blog post
tar_target(
  "blog",
  tar_quarto(
    "output/blog.html",
    "posts/post-with-code/index.qmd",
    deps = tar_target("triangles")
  )
)

