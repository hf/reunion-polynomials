package me.stojan.reunion.polynomials

import me.stojan.reunion.structure.Field

import me.stojan.polynome.{ Polynomial, NullPolynomial }

/**
 * A polynomial with generic coefficients which are a field.
 */
private case class FPolynomial[V](degree: Long, coefficient: Field[V], override val sub: Polynomial[Field[V]]) extends Polynomial[Field[V]] {
  override def +(p: Polynomial[Field[V]]): Polynomial[Field[V]] =
    if (degree < p.degree) {
      p + this
    } else {
      if (degree == p.degree) {
        val c = coefficient + p.coefficient

        if (c.isZero) {
          sub + p.sub
        } else {
          FPolynomial(degree, c, sub + p.sub)
        }
      } else {
        FPolynomial(degree, coefficient, sub + p)
      }
    }

  override def *+(p: Polynomial[Field[V]]): Polynomial[Field[V]] =
    FPolynomial(degree + p.degree, coefficient * p.coefficient, this.sub *+ p)

  override def /-(p: Polynomial[Field[V]]): Polynomial[Field[V]] =
    // coefficient / p.coefficient will never be 0 since coefficient must not be 0
    FPolynomial(degree - p.degree, coefficient / p.coefficient, NullPolynomial[Field[V]]())

  override def unary_-(): Polynomial[Field[V]] =
    FPolynomial(degree, -coefficient, -sub)
}

/**
 * Contains helper functions for polynomials with field coefficients.
 */
object FieldPolynomial {
  /**
   * Returns a `NullPolynomial[Field[V]]`.
   */
  def apply[V](): Polynomial[Field[V]] = NullPolynomial[Field[V]]()

  /**
   * Returns a new polynomial which is the last term in the polynomial sequence.
   */
  def apply[V](degree: Long, coefficient: Field[V]): Polynomial[Field[V]] = FPolynomial(degree, coefficient, NullPolynomial[Field[V]]())

  /**
   * Returns a new polynomial which is the first term in the sequence.
   */
  def apply[V](degree: Long, coefficient: Field[V], sub: Polynomial[Field[V]]): Polynomial[Field[V]] = FPolynomial(degree, coefficient, sub)

  /**
   * Checks whether the polynomial is zero.
   */
  def isZero[V](polynomial: Polynomial[Field[V]]): Boolean = polynomial.degree < 0

  /**
   * Checks whether the polynomial is one.
   */
  def isOne[V](polynomial: Polynomial[Field[V]]): Boolean = polynomial.degree == 0 && polynomial.coefficient.isOne

  /**
   * Checks whether `a` is greater than (>) `b`.
   */
  def greater[V](a: Polynomial[Field[V]], b: Polynomial[Field[V]]): Boolean =
    (a.degree > b.degree) || (a.degree == b.degree && a.degree > -1 && (a.coefficient > b.coefficient || FieldPolynomial.greater(a.sub, b.sub)))

  /**
   * Checks whether `a` is less than (<) `b`.
   */
  def lesser[V](a: Polynomial[Field[V]], b: Polynomial[Field[V]]): Boolean =
    (a.degree < b.degree) || (a.degree == b.degree && a.degree > -1 && (a.coefficient < b.coefficient || FieldPolynomial.lesser(a.sub, b.sub)))
}
