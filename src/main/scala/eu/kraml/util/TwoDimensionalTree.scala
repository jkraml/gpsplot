package eu.kraml.util

import scala.math.{max, min}


object TwoDimensionalTree {

    class BoundingBox(private val minX: Int,
                      private val maxX: Int,
                      private val minY: Int,
                      private val maxY: Int) {
        if ((minX > maxX) || (minY > maxY))
            throw new IllegalArgumentException("invalid bounds")

        def overlaps(other: BoundingBox): Boolean = {
            overlaps(this.minX, this.maxX, other.minX, other.maxX) ||
                overlaps(this.minY, this.maxY, other.minY, other.maxY)
        }

        private def overlaps(thisMin: Int, thisMax: Int, otherMin: Int, otherMax: Int): Boolean = {
            !((thisMax < otherMin) || (thisMin > otherMax))
        }

        def contains(other: BoundingBox): Boolean = {
            contains(this.minX, this.maxX, other.minX, other.maxX) &&
                contains(this.minY, this.maxY, other.minY, other.maxY)
        }

        private def contains(thisMin: Int, thisMax: Int, otherMin: Int, otherMax: Int): Boolean = {
            (thisMin <= otherMin) && (otherMax <= thisMax)
        }

        def combine(other: BoundingBox): BoundingBox = {
            new BoundingBox(
                min(this.minX, other.minX),
                max(this.maxX, other.maxX),
                min(this.minY, other.minY),
                max(this.maxY, other.maxY)
            )
        }
    }

    sealed trait Tree[Value] {
        def get(bounds: BoundingBox): Stream[Value]

        def getAll: Stream[Value]

        def boundingBox: BoundingBox

        def size: Int
        def height: Int
    }

    private class Node[Value](private val children: Seq[Tree[Value]]) extends Tree[Value] {

        val boundingBox: BoundingBox = children.map(c => c.boundingBox).reduceLeft((a, b) => a combine b)

        override def get(bounds: BoundingBox): Stream[Value] = {
            if (bounds.contains(boundingBox))
                return getAll

            if (!bounds.overlaps(boundingBox))
                return Stream.empty

            children.flatMap(_.get(bounds)).toStream
        }

        override def getAll: Stream[Value] = children.flatMap(_.getAll).toStream

        override val size: Int = 1 + children.map(_.size).sum

        override val height: Int = 1 + children.map(_.height).max
    }

    private class Leaf[Value](private val x: Int,
                              private val y: Int,
                              private val value: Value) extends Tree[Value] {

        //does not check bounds
        override def get(bounds: BoundingBox): Stream[Value] = {
            if (bounds.overlaps(boundingBox))
                Stream(value)
            else
                Stream.empty
        }

        override def getAll: Stream[Value] = Stream(value)

        override val boundingBox: BoundingBox = new BoundingBox(x, x, y, y)

        override val size: Int = 1

        override val height: Int = 1
    }

    def makeTree[Value](collection: Traversable[((Int, Int), Value)]): Tree[Value] = {
        if (collection.size == 1)
            makeLeaf(collection.head)
        else
            makeNode(collection)
    }

    private def makeLeaf[Value](record: ((Int, Int), Value)): Leaf[Value] = {
        record match {
            case ((x, y), v) => new Leaf[Value](x, y, v)
        }
    }

    private def makeNode[Value](collection: Traversable[((Int, Int), Value)]): Tree[Value] = {
        val (xSplit, xScore) = getSplitValue(collection.map({ case ((x, y), v) => x }).toList)
        val (ySplit, yScore) = getSplitValue(collection.map({ case ((x, y), v) => y }).toList)

        val filterFunc: (((Int, Int), Value)) => Boolean =
            if (xScore > yScore)
                { case (((x, y), v)) => x <= xSplit }
            else
                { case (((x, y), v)) => y <= ySplit }

        val (part1, part2) = collection.partition(filterFunc)

        new Node(List(makeTree(part1), makeTree(part2)))
    }

    def getSplitValue(sortedValues: List[Int]): (Int, Double) = {
        val counts = sortedValues.groupBy(identity).mapValues(_.size).toList.sortBy(_._1)

        var sum = 0
        val cumulative = counts.map {
            case (x, count) =>
                sum += count
                (x, sum)
        }

        // scores are better the closer the cumulative sum is to 50% of the collection size
        val scores = cumulative.map {
            case (x, partialSum) =>
                val fraction = partialSum.toDouble / sum
                (x, .5-math.abs(fraction - .5))
        }

        scores.max(Ordering[Double].on[(Int, Double)](_._2))
    }
}
