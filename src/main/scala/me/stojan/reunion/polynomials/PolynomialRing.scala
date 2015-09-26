package me.stojan.reunion.polynomials

import me.stojan.reunion.structure.{ Field, Ring }
import me.stojan.reunion.euclidean.Euclidean

import me.stojan.polynome.Polynomial

/**
 * Contains functions for managing polynomial rings, i.e. polynomials with coefficients in a field.
 */
object PolynomialRing {
  /**
   * Wrapper class that exposes `Polynomial[Field[V]]` as a `Ring`.
   */
  private case class FPRing[V](value: Polynomial[Field[V]]) extends Ring[Polynomial[Field[V]]] {
    override def +(r: Ring[Polynomial[Field[V]]]): Ring[Polynomial[Field[V]]] = value + r.value
    override def -(r: Ring[Polynomial[Field[V]]]): Ring[Polynomial[Field[V]]] = value - r.value
    override def unary_-(): Ring[Polynomial[Field[V]]] = -value

    override val isZero: Boolean = FieldPolynomial.isZero(value)
    override val isOne: Boolean = FieldPolynomial.isOne(value)

    override def *(r: Ring[Polynomial[Field[V]]]): Ring[Polynomial[Field[V]]] = value * r.value

    override def >(r: Ring[Polynomial[Field[V]]]): Boolean = FieldPolynomial.greater(value, r.value)
    override def <(r: Ring[Polynomial[Field[V]]]): Boolean = FieldPolynomial.lesser(value, r.value)

    private implicit def polynomialToRing(p: Polynomial[Field[V]]): Ring[Polynomial[Field[V]]] = FPRing(p)
  }

  /**
   * Converts a polynomial with coefficients in a generic field to a ring.
   */
	def ring[V](polynomial: Polynomial[Field[V]]): Ring[Polynomial[Field[V]]] = FPRing(polynomial)

  /**
   * Wrapper class that exposes `Polynomial[Field[V]]` as a `Euclidean`.
   */
  private case class FPEuclidean[V](value: Polynomial[Field[V]]) extends Euclidean[Polynomial[Field[V]]] {
    override def +(e: Euclidean[Polynomial[Field[V]]]): Euclidean[Polynomial[Field[V]]] = value + e.value
    override def -(e: Euclidean[Polynomial[Field[V]]]): Euclidean[Polynomial[Field[V]]] = value - e.value
    override def unary_-(): Euclidean[Polynomial[Field[V]]] = -value

    override val isZero: Boolean = FieldPolynomial.isZero(value)
    override val isOne: Boolean = FieldPolynomial.isOne(value)
    
    override def *(e: Euclidean[Polynomial[Field[V]]]): Euclidean[Polynomial[Field[V]]] = value * e.value

    override def /(e: Euclidean[Polynomial[Field[V]]]): Euclidean[Polynomial[Field[V]]] = 
      // euclidean divison quotient
      (value / e.value)._1

    override def >(e: Euclidean[Polynomial[Field[V]]]): Boolean = FieldPolynomial.greater(value, e.value)
    override def <(e: Euclidean[Polynomial[Field[V]]]): Boolean = FieldPolynomial.lesser(value, e.value)

    implicit private def polynomialToEuclidean(p: Polynomial[Field[V]]): Euclidean[Polynomial[Field[V]]] = FPEuclidean(p)
  }

  /**
   * Converts a polynomial with coefficients in a generic field to a euclidean.
   */
  def euclidean[V](polynomial: Polynomial[Field[V]]): Euclidean[Polynomial[Field[V]]] = FPEuclidean(polynomial)
}