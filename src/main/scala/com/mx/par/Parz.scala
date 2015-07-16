package com.mx.par

import com.mx.fp.Par.Par
import com.mx.fp.core.stackless.Free

//free par
trait Parz[A] extends Free[Par,A]
