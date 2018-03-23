# Proto Parser

Usage: `protop "4 4 8 16 16 1 1 1 13 bits" path/to/hexdump

## Motivation

Hexdumps typically look like this

```
05:39:52.735660 IP (tos 0x0, ttl 60, id 37103, offset 0, flags [none],
	proto UDP (17), length 141)

  0x0000:  4500 008d 90ef 0000 3c11 44e3 0808 0808  E.......<.D.....
  ...
```

Which is useful, but often times I'll be looking through it with a protocol definition, like:

![ip header]()

You can see in the above (from tcpdump), some of this information is parsed out (like fragment id =
37103). It'd be convenient to have a tool that, given a spec and a hexdump, would parse out the
fields so the developer doesn't have to do it visually.
