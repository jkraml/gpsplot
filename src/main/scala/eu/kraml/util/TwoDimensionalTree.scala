package eu.kraml.util

import scala.math.{max, min}


object TwoDimensionalTree {

    case class BoundingBox(minX: Int,
                           maxX: Int,
                           minY: Int,
                           maxY: Int) {
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

        def contains(point: (Int, Int)): Boolean = {
            val (x, y) = point
            contains(this.minX, this.maxX, x, x) &&
                contains(this.minY, this.maxY, y, y)
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

        def include(point: (Int, Int)): BoundingBox = {
            val (x, y) = point
            new BoundingBox(
                min(this.minX, x),
                max(this.maxX, x),
                min(this.minY, y),
                max(this.maxY, y)
            )
        }

        //TODO is there a better name?
        def includeBorder(margin: Int): BoundingBox = {
            new BoundingBox(
                minX - margin,
                maxX + margin,
                minY - margin,
                maxY + margin
            )
        }
    }

    object BoundingBox {
        def around(values: Traversable[(Int, Int)]): BoundingBox = {
            val (initX, initY) = values.head
            val initBox = new BoundingBox(initX, initX, initY, initY)

            values.foldLeft(initBox)(_ include _)
        }
    }

    sealed trait Tree[Value] {
        def get(bounds: BoundingBox): Stream[((Int, Int), Value)]

        def getValues(bounds: BoundingBox): Stream[Value] = {
            get(bounds).map(_._2)
        }

        def getAll: Stream[((Int, Int), Value)]

        def getAllValues: Stream[Value] = {
            getAll.map(_._2)
        }

        def boundingBox: BoundingBox

        def size: Int

        def height: Int
    }

    private class Node[Value](private val children: Seq[Tree[Value]]) extends Tree[Value] {

        val boundingBox: BoundingBox = children.map(_.boundingBox).reduceLeft((a, b) => a combine b)

        override def get(bounds: BoundingBox): Stream[((Int, Int), Value)] = {
            if (bounds.contains(boundingBox))
                return getAll

            if (!bounds.overlaps(boundingBox))
                return Stream.empty

            children.flatMap(_.get(bounds)).toStream
        }

        override def getAll: Stream[((Int, Int), Value)] = children.flatMap(_.getAll).toStream

        override val size: Int = 1 + children.map(_.size).sum

        override val height: Int = 1 + children.map(_.height).max
    }

    private class MultiLeaf[Value](values: Traversable[((Int, Int), Value)]) extends Tree[Value] {
        override val boundingBox: BoundingBox = BoundingBox.around(values.map(_._1))

        override def get(bounds: BoundingBox): Stream[((Int, Int), Value)] =
            values.filter(v => {
                bounds.contains(v._1)
            }).toStream

        override def getAll: Stream[((Int, Int), Value)] = values.toStream

        override def size: Int = values.size

        override def height: Int = 1
    }

    def makeTree[Value](collection: Traversable[((Int, Int), Value)]): Tree[Value] = {
        if (collection.size <= 16)
            makeMultiLeaf(collection)
        else
            makeNode(collection)
    }

    private def makeMultiLeaf[Value](collection: Traversable[((Int, Int), Value)]): Tree[Value] = {
        new MultiLeaf[Value](collection)
    }

    private def makeNode[Value](collection: Traversable[((Int, Int), Value)]): Tree[Value] = {
        val (xSplit, xScore) = getSplitValue(collection.map({ case ((x, y), v) => x }).toList)
        val (ySplit, yScore) = getSplitValue(collection.map({ case ((x, y), v) => y }).toList)

        val filterFunc: (((Int, Int), Value)) => Boolean =
            if (xScore > yScore) {
                case (((x, y), v)) => x <= xSplit
            }
            else {
                case (((x, y), v)) => y <= ySplit
            }

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
                (x,.5 - math.abs(fraction - .5))
        }

        scores.max(Ordering[Double].on[(Int, Double)](_._2))
    }
}
