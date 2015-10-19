package me.stojan.reunion.polynomials

import me.stojan.reunion.structure.{ Field, Ring, FieldDescriptor, RingDescriptor }
import me.stojan.reunion.euclidean.{ Euclidean, EuclideanDescriptor }

import me.stojan.polynome.Polynomial

/**
 * Contains functions for managing polynomial rings, i.e. polynomials with coefficients in a field.
 */
object PolynomialRing {

  private case class FPRingDescriptor[V](coefficientDescriptor: FieldDescriptor[V]) extends RingDescriptor[Polynomial[Field[V]]] {
    override def obtain(value: Polynomial[Field[V]]): Ring[Polynomial[Field[V]]] = FPRing(value, coefficientDescriptor)

    override lazy val one: Ring[Polynomial[Field[V]]] = obtain(FieldPolynomial(0, coefficientDescriptor.one))
    override lazy val zero: Ring[Polynomial[Field[V]]] = obtain(FieldPolynomial())
  }

  /**
   * Wrapper class that exposes `Polynomial[Field[V]]` as a `Ring`.
   */
  private case class FPRing[V](value: Polynomial[Field[V]], coefficientDescriptor: FieldDescriptor[V]) extends Ring[Polynomial[Field[V]]] {
    override val descriptor: RingDescriptor[Polynomial[Field[V]]] = FPRingDescriptor(coefficientDescriptor)

    override def +(r: Ring[Polynomial[Field[V]]]): Ring[Polynomial[Field[V]]] = value + r.value
    override def -(r: Ring[Polynomial[Field[V]]]): Ring[Polynomial[Field[V]]] = value - r.value
    override def unary_-(): Ring[Polynomial[Field[V]]] = -value

    override val isZero: Boolean = FieldPolynomial.isZero(value)
    override val isOne: Boolean = FieldPolynomial.isOne(value)

    override def *(r: Ring[Polynomial[Field[V]]]): Ring[Polynomial[Field[V]]] = value * r.value

    override def >(r: Ring[Polynomial[Field[V]]]): Boolean = FieldPolynomial.greater(value, r.value)
    override def <(r: Ring[Polynomial[Field[V]]]): Boolean = FieldPolynomial.lesser(value, r.value)

    private implicit def polynomialToRing(p: Polynomial[Field[V]]): Ring[Polynomial[Field[V]]] = FPRing(p, coefficientDescriptor)
  }

  /**
   * Converts a polynomial with coefficients in a generic field to a ring.
   */
	def ring[V](polynomial: Polynomial[Field[V]])(implicit coefficientDescriptor: FieldDescriptor[V]): Ring[Polynomial[Field[V]]] = FPRing(polynomial, coefficientDescriptor)

  private case class FPEuclideanDescriptor[V](coefficientDescriptor: FieldDescriptor[V]) extends EuclideanDescriptor[Polynomial[Field[V]]] {
    override def obtain(value: Polynomial[Field[V]]): Euclidean[Polynomial[Field[V]]] = FPEuclidean(value, coefficientDescriptor)

    override lazy val one: Euclidean[Polynomial[Field[V]]] = obtain(FieldPolynomial(0, coefficientDescriptor.one))
    override lazy val zero: Euclidean[Polynomial[Field[V]]] = obtain(FieldPolynomial())
  }

  /**
   * Wrapper class that exposes `Polynomial[Field[V]]` as a `Euclidean`.
   */
  private case class FPEuclidean[V](value: Polynomial[Field[V]], coefficientDescriptor: FieldDescriptor[V]) extends Euclidean[Polynomial[Field[V]]] {
    override val descriptor: EuclideanDescriptor[Polynomial[Field[V]]] = FPEuclideanDescriptor(coefficientDescriptor)

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

    implicit private def polynomialToEuclidean(p: Polynomial[Field[V]]): Euclidean[Polynomial[Field[V]]] = FPEuclidean(p, coefficientDescriptor)
  }

  /**
   * Converts a polynomial with coefficients in a generic field to a euclidean.
   */
  def euclidean[V](polynomial: Polynomial[Field[V]])(implicit coefficientDescriptor: FieldDescriptor[V]): Euclidean[Polynomial[Field[V]]] = FPEuclidean(polynomial, coefficientDescriptor)
}
