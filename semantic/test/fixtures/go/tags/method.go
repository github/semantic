import "net/http"

func (c *apiClient) CheckAuth(req *http.Request, user, repo string) (*authenticatedActor, error) {}
