docker run --rm \
  -v ${PWD}:/local openapitools/openapi-generator-cli generate \
  -i /local/openapi.yaml \
  -g haskell-http-client \
  -o /local/out/haskell-client