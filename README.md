# stripe-servant

The Stripe API (partially) defined in Haskell as a Servant API type


### :bangbang: :fire: :warning: :fire: USE [dmjio/stripe](https://github.com/dmjio/stripe) INSTEAD :fire: :warning: :fire: :bangbang:

You're almost certainly looking for [dmjio/stripe](https://github.com/dmjio/stripe), but I started this project because I needed two things that it didn't offer:

1. thorough Stripe Connect support
2. ACH

I also wanted to explore the use of [`servant-client`](https://hackage.haskell.org/package/servant-client) in the process of implementing those, so here we are.

The project I was building this for was eventually canceled though, so it should also be noted that this logic has never been used in anger.

Consider yourself disclaimed!


## Stripe Version

This package currently targets Stripe API version [`2017-08-15`](https://stripe.com/docs/upgrades#2017-08-15) and you can find copies of the documentation for that Stripe version in [`bchase/stripe-docs`](https://github.com/bchase/stripe-docs/tree/master/v2017-08-15).

A `Version` data type is also provided, but it currently only serves to set the [`Stripe-Version`](https://stripe.com/docs/api#versioning) header appropriately.


## Usage

[`app/Main.hs`](https://github.com/bchase/stripe-servant/blob/master/app/Main.hs) contains some example usage:

```haskell
createAndChargeAndDeleteCustomer :: Stripe (Customer, Charge, [Charge], Bool)
createAndChargeAndDeleteCustomer = do
  cust    <- stripe WithoutConnect . createCustomer $ custReq
  charge  <- stripe WithoutConnect . createCharge   $ chargeReq cust

  charges <- stripe WithoutConnect . paginate [ By 10 ] $ listCharges

  deleted <- fmap (destroyDeleted . stripeMetadata) . stripe' WithoutConnect . destroyCustomer $ customerId cust

  return (cust, charge, charges, deleted)

  where
    custReq = (customerCreateReq (Token "tok_visa")) { customerCreateEmail = Just "test@example.com" }

    chargeReq Customer{customerId} = chargeCreateReq (Price USD 10000) (SCustomer customerId)


main :: IO ()
main = do
  let ver    = Version'2017'08'15
      key    = SecretKey "sk_test_BQokikJOvBiI2HlWgH4olfQ2"
      config = Config ver key

  eResp <- stripeIO config createAndChargeAndDeleteCustomer

  case eResp of
    Left  (StripeErrorResponse   err  ) -> putStrLn "Stripe Error:"     >> print err
    Left  (StripeDecodeFailure   err _) -> putStrLn "Decode Failure:"   >> print err
    Left  (StripeConnectionError err  ) -> putStrLn "Connection Error:" >> print err
    Right (_, charge, _, _)             -> print charge
```

This can be compiled and run with [Stack](https://docs.haskellstack.org/en/stable/README/):

```
$ stack build && stack exec stripe-servant-exe
```


### Usage Explanation

Let's take a look at some of these functions step-by-step.

First, we generate `createCharge` with, essentially:

```haskell
type ChargeCreate = -- POST https://api.stripe.com/v1/charges
  "v1" :> "charges"
    :> ReqBody '[FormUrlEncoded] ChargeCreateReq
    :> Header "Authorization"  SecretKey
    :> Header "Stripe-Version" Version
    :> Header "Stripe-Account" AccountId -- Connect account (optional)
    :> Post '[JSON] (ScalarResp Charge)

createCharge :: ChargeCreateReq -> Client (ScalarResp Charge)
createCharge = Servant.Client.client (Proxy :: Proxy ChargeCreate)
```


But in the actual source, the above collapses to:

```haskell
type ChargeCreate = "v1" :> "charges" :> Body ChargeCreateReq :> Post' Charge

createCharge :: Create ChargeCreateReq Charge
createCharge = Servant.Client.client (Proxy :: Proxy ChargeCreate)



-- using the `type` synonyms...

---- REQUEST ----

type ReqHeaders a =
     Header "Authorization"  SecretKey
  :> Header "Stripe-Version" Version
  :> Header "Stripe-Account" AccountId
  :> a

type Post' a = ReqHeaders (Post '[JSON] (ScalarResp  a))

type Body t = ReqBody '[FormUrlEncoded] t


---- RESPONSE ----

type RespHeaders a = Headers '[Header "Request-Id" String] a

type ScalarResp a = RespHeaders (ScalarJSON  a)


---- ACTION ----

type Create req resp = req -> Client (ScalarResp resp)
```

Here we've used [`Servant.Client.client`](https://hackage.haskell.org/package/servant-client-0.13.0.1/docs/Servant-Client.html#v:client) to generate functions that will provide us with a `Client resp`. Here's the type signature again, for reference:

```haskell
createCharge :: ChargeCreateReq -> Client (ScalarResp Charge)
```

This particular `Client` contains a `ScalarResp a`, which is the raw Servant [`Headers`](http://hackage.haskell.org/package/servant-0.13.0.1/docs/Servant-API-ResponseHeaders.html#t:Headers), but this will eventually be converted into a `Resp a` for us.

Based on the kind of request, the `resp` in `Client (resp a)` will vary among these three:

```haskell
type ScalarResp   a = RespHeaders (ScalarJSON  a)
type ListResp     a = RespHeaders (ListJSON    a)
type DestroyResp id = RespHeaders (DeleteJSON id)
```

But these types only matter in regard to the API type definitions; as far as manipulating the data goes, you'll end up with a `Resp a` in hand:

```haskell
data Resp a = Resp
  { stripeRequestId :: String
  , stripeMetadata  :: RespMetadata
  , stripeData      :: a
  } deriving ( Show, Generic, Functor )

data RespMetadata
  = ScalarMeta
  | ListMeta    { listHasMore    :: Bool }
  | DestroyMeta { destroyDeleted :: Bool }
  deriving ( Show, Generic )
```

Here we see that responses  _always_ include a request ID and data, but list and destroy actions will also contain some metadata.

Moving on, we'll get ourselves into the `Stripe` monad:

```haskell
-- if we're interested in the req ID or metadata:
stripe' :: ( RespBody rb ) => Connect -> Client (RespHeaders (rb a)) -> Stripe (Resp a)

-- otherwise, we can go straight to `Stripe a`:
stripe  :: ( RespBody rb ) => Connect -> Client (RespHeaders (rb a)) -> Stripe a
```

> The [`Stripe` monad](https://github.com/bchase/stripe-servant/blob/72535b1bc776b3b298a00277d9f068a5e6e43bfc/src/Stripe/Types.hs#L181-L182) in this package is modeled closely after [`AppT`](https://github.com/parsonsmatt/servant-persistent/blob/744e3960d23642466d9eca784853ac709e930360/src/Config.hs#L36-L40) from [parsonsmatt/servant-persistent](https://github.com/parsonsmatt/servant-persistent).

And finally, we'll want to use these to actually hit the Stripe API, by running them in `IO`:

```haskell
stripeIO :: Config -> Stripe a -> IO (Either StripeFailure a)
```

All together now!

```haskell
test :: IO (Either StripeFailure Customer)
test = do
  let cfg = Config Version'2017'08'15 $ SecretKey "sk_test_BQokikJOvBiI2HlWgH4olfQ2"
      req = customerCreateReq $ Token "tok_visa"
  stripeIO cfg . stripe WithoutConnect . createCustomer $ req
```


## Resources

### Charges

- [X] create
- [X] read
- [X] update
- [X] list
- [X] capture

### Customers

- [X] create
- [X] read
- [X] update
- [X] destroy
- [X] list

### Cards

- [X] create
- [X] read
- [X] update
- [X] destroy
- [X] list

### Bank Accounts

- [X] create
- [X] read
- [X] update
- [X] destroy
- [X] list
- [X] verify

### Plans

- [X] create
- [X] read
- [X] update
- [X] destroy
- [X] list
