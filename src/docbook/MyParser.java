package MyParser;

import fj.P2;
import fj.F;
import fj.P1;
import static fj.pre.Equal.charEqual;
import static fj.P.p;
import fj.data.Option;
import fj.data.List;
import static fj.data.Validation.parseInt;
import static fj.data.List.asString;
import static fj.data.List.fromString;
import static fj.data.Option.some;
import static fj.data.Option.none;

import static java.lang.Character.*;
import static MyParser.Parser.*;

abstract class Parser<A> {
  abstract Option<P2<List<Character>, A>> parse(List<Character> s);

  Parser<A> or(final P1<Parser<A>> p2) {
    return new Parser<A>() {
      Option<P2<List<Character>, A>> parse(final List<Character> s) {
        final Option<P2<List<Character>, A>> v = Parser.this.parse(s);
        return v.isSome()
            ? v
            : p2._1().parse(s);
      }
    };
  }

  <B> Parser<B> anonymousBind(final P1<Parser<B>> q) {
    return bindParser(this, new F<A, Parser<B>>() {
      public Parser<B> f(final A _) {
        return q._1();
      }
    });
  }

  static <A> Parser<A> value(final A a) {
    return new Parser<A>() {
      Option<P2<List<Character>, A>> parse(final List<Character> s) {
        return some(p(s, a));
      }
    };
  }

  static <A> Parser<A> failed() {
    return new Parser<A>() {
      Option<P2<List<Character>, A>> parse(final List<Character> s) {
        return none();
      }
    };
  }

  static Parser<Character> character() {
    return new Parser<Character>() {
      Option<P2<List<Character>, Character>> parse(final List<Character> s) {
        return s.isEmpty()
            ? Option.<P2<List<Character>, Character>>none()
            : some(p(s.tail(), s.head()));
      }
    };
  }

  static <A, B> Parser<B> mapParser(final Parser<A> p, final F<A, B> f) {
    return new Parser<B>() {
      Option<P2<List<Character>, B>> parse(final List<Character> s) {
        return p.parse(s).map(new F<P2<List<Character>, A>, P2<List<Character>, B>>() {
          public P2<List<Character>, B> f(final P2<List<Character>, A> rc) {
            return rc.map2(f);
          }
        });
      }
    };
  }

  static <A, B> Parser<B> bindParser(final Parser<A> p, final F<A, Parser<B>> f) {
    return new Parser<B>() {
      Option<P2<List<Character>, B>> parse(final List<Character> s) {
        return p.parse(s).bind(new F<P2<List<Character>, A>, Option<P2<List<Character>, B>>>() {
          public Option<P2<List<Character>, B>> f(final P2<List<Character>, A> rc) {
            return f.f(rc._2()).parse(rc._1());
          }
        });
      }
    };
  }

  static <A> Parser<List<A>> sequenceParser(final List<Parser<A>> ps) {
    return ps.isEmpty()
        ? value(List.<A>nil())
        : bindParser(ps.head(), new F<A, Parser<List<A>>>() {
      public Parser<List<A>> f(final A a) {
        return mapParser(sequenceParser(ps.tail()), List.<A>cons().f(a));
      }
    });
  }

  static <A> Parser<List<A>> thisMany(final int n, final Parser<A> p) {
    return sequenceParser(List.replicate(n, p));
  }

  static <A> Parser<List<A>> list(final Parser<A> k) {
    return many1(k).or(new P1<Parser<List<A>>>() {
      public Parser<List<A>> _1() {
        return value(List.<A>nil());
      }
    });
  }

  static <A> Parser<List<A>> many1(final Parser<A> k) {
    return bindParser(k, new F<A, Parser<List<A>>>() {
      public Parser<List<A>> f(final A kx) {
        return mapParser(list(k), List.<A>cons().f(kx));
      }
    });
  }

  static Parser<Character> satisfy(final F<Character, Boolean> p) {
    return bindParser(character(), new F<Character, Parser<Character>>() {
      public Parser<Character> f(final Character c) {
        return p.f(c) ? value(c) : Parser.<Character>failed();
      }
    });
  }

  static Parser<Character> is(final char c) {
    return satisfy(charEqual.eq(c));
  }

  final static Parser<Character> digit = satisfy(new F<Character, Boolean>() {
    public Boolean f(final Character z) {
      return isDigit(z);
    }
  });

  final static Parser<Integer> natural = mapParser(list(digit), new F<List<Character>, Integer>() {
    public Integer f(final List<Character> z) {
      return parseInt(asString(z)).success();
    }
  });

  final static Parser<Character> space = satisfy(new F<Character, Boolean>() {
    public Boolean f(final Character z) {
      return isWhitespace(z);
    }
  });

  final static Parser<List<Character>> spaces = many1(space);

  final static Parser<Character> lower = satisfy(new F<Character, Boolean>() {
    public Boolean f(final Character z) {
      return isLowerCase(z);
    }
  });

  final static Parser<Character> upper = satisfy(new F<Character, Boolean>() {
    public Boolean f(final Character z) {
      return isUpperCase(z);
    }
  });

  final static Parser<Character> alpha = satisfy(new F<Character, Boolean>() {
    public Boolean f(final Character z) {
      return isLetter(z);
    }
  });

  final static Parser<Character> alphaNum = satisfy(new F<Character, Boolean>() {
    public Boolean f(final Character z) {
      return isLetterOrDigit(z);
    }
  });
}

class Person {
  final int age;
  final String firstName;
  final String surname;
  final char gender;
  final String phone;

  Person(final int age, final String firstName, final String surname, final char gender, final String phone) {
    this.age = age;
    this.firstName = firstName;
    this.surname = surname;
    this.gender = gender;
    this.phone = phone;
  }

  final static Parser<Integer> ageParser = natural;

  final static Parser<String> firstNameParser = bindParser(upper, new F<Character, Parser<String>>() {
    public Parser<String> f(final Character c) {
      return mapParser(list(lower), new F<List<Character>, String>() {
        public String f(final List<Character> cs) {
          return asString(cs.cons(c));
        }
      });
    }
  });

  final static Parser<String> surnameParser = bindParser(upper, new F<Character, Parser<String>>() {
    public Parser<String> f(final Character c) {
      return bindParser(thisMany(5, lower), new F<List<Character>, Parser<String>>() {
        public Parser<String> f(final List<Character> cs) {
          return mapParser(list(lower), new F<List<Character>, String>() {
            public String f(final List<Character> t) {
              return asString(cs.cons(c).append(t));
            }
          });
        }
      });
    }
  });

  final static Parser<Character> genderParser = is('m').or(new P1<Parser<Character>>() {
    public Parser<Character> _1() {
      return is('f');
    }
  });

  final static Parser<String> phoneBodyParser = mapParser(list(digit.or(new P1<Parser<Character>>() {
    public Parser<Character> _1() {
      return is('.');
    }
  }).or(new P1<Parser<Character>>() {
    public Parser<Character> _1() {
      return is('-');
    }
  })), new F<List<Character>, String>() {
    public String f(final List<Character> z) {
      return asString(z);
    }
  });

  final static Parser<String> phoneParser = bindParser(digit, new F<Character, Parser<String>>() {
    public Parser<String> f(final Character d) {
      return bindParser(phoneBodyParser, new F<String, Parser<String>>() {
        public Parser<String> f(final String z) {
          return mapParser(is('#'), new F<Character, String>() {
            public String f(final Character _) {
              return asString(fromString(z).cons(d));
            }
          });
        }
      });
    }
  });

  final static Parser<Person> personParser1 = bindParser(ageParser, new F<Integer, Parser<Person>>() {
    public Parser<Person> f(final Integer age) {
      return spaces.anonymousBind(new P1<Parser<Person>>() {
        public Parser<Person> _1() {
          return bindParser(firstNameParser, new F<String, Parser<Person>>() {
            public Parser<Person> f(final String firstName) {
              return spaces.anonymousBind(new P1<Parser<Person>>() {
                public Parser<Person> _1() {
                  return bindParser(surnameParser, new F<String, Parser<Person>>() {
                    public Parser<Person> f(final String surname) {
                      return spaces.anonymousBind(new P1<Parser<Person>>() {
                        public Parser<Person> _1() {
                          return bindParser(genderParser, new F<Character, Parser<Person>>() {
                            public Parser<Person> f(final Character gender) {
                              return spaces.anonymousBind(new P1<Parser<Person>>() {
                                public Parser<Person> _1() {
                                  return bindParser(phoneParser, new F<String, Parser<Person>>() {
                                    public Parser<Person> f(final String phone) {
                                      return value(new Person(age, firstName, surname, gender, phone));
                                    }
                                  });
                                }
                              });
                            }
                          });
                        }
                      });
                    }
                  });
                }
              });
            }
          });
        }
      });
    }
  });
}