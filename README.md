# servant-cookie-hmac-auth-example

This example builds on [servant-persistent](https://github.com/parsonsmatt/servant-persistent) and
adds:
 * cookie authentication combinator [CookieAuth](src/CookieAuth.hs)
 * request body authentication combinator [AuthReqBody](src/AuthReqBody.hs)
 * simple login [handler](src/Api/Session.hs)

## Compiling and starting the server

```
createuser -P example # set password to example
createdb -O example example
stack install
stack exec backend-exe
```


## Example requests

Login:
```
curl -v -XPOST  \
    http://localhost:8081/login \
    --data '{"username":"Admin","password":"Admin"}' \
    -H "Content-Type: application/json"
```

Cookie-authenticated request:
```
curl -XPOST \
    http://localhost:8081/cookieauth \
    -H "Cookie: _SESSION=RkxNYS9sbkF2MVNiWSt3bUZiWm1zSUprZDc4UDE3VDE1ODBseHhSOENnWkRaZFZPTVBseE5wYUsvZ3N4TmV1aFQ5VDBBa3B3UDJJOVJZdUdnNjh4cTlYTWhRZVg5RWdjQmFvUWxlTldONnVoQ25VaDlzL2dIcUQyVWhnTU5SREpkelBuNENDNlp1WmxwQT09" \
    -H "Content-Type: application/json" \
    --data '{"values":[1,2,3]}'
```

Authenticated request body with access key 1234 and corresponding password 4321.
```
curl -XPOST \
    http://localhost:8081/authbody \
    --data '{"values":[1,2,3]}' \
    -H 'Authorization: Example 1234:f5bff3bb40ec5b994302dda447378b46e7ae0d3d2aefe7eb094c8b936269f1a3' \
    -H 'Content-Type: application/json'
```


