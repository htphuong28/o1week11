package o1.adventure

import scala.collection.mutable.Map

/** A `Player` object represents a player character controlled by the real-life user
  * of the program.
  *
  * A player object’s state is mutable: the player’s location and possessions can change,
  * for instance.
  *
  * @param startingArea  the player’s initial location */
class Player(startingArea: Area):

  private var currentLocation = startingArea        // gatherer: changes in relation to the previous location
  private var quitCommandGiven = false              // one-way flag
  private val bag = Map[String, Item]()

  /** Determines if the player has indicated a desire to quit the game. */
  def hasQuit = this.quitCommandGiven

  /** Returns the player’s current location. */
  def location = this.currentLocation

  def drop(itemName: String): String =
    bag.remove(itemName) match
      case Some(thing) =>

        currentLocation.addItem(thing)
        s"You drop the $itemName."
      case None => "You don't have that!"

  def examine(itemName: String): String =
    bag.get(itemName) match
      case Some(item) => s"You look closely at the $itemName.\n${item.description}"
      case None => "If you want to examine something, you need to pick it up first."

  def get(itemName: String): String =
    currentLocation.removeItem(itemName) match
      case Some(item) =>
        bag += item.name -> item
        "You pick up the " + itemName

      case None => "There is no "+ itemName + " here to pick up."

  def inventory: String =
    if bag.nonEmpty then "You are carrying:\n"+ bag.keys.mkString("\n") else "You are empty-handed."

  def has(itemName: String): Boolean = this.bag.contains(itemName)

  /** Attempts to move the player in the given direction. This is successful if there
    * is an exit from the player’s current location towards the direction name. Returns
    * a description of the result: "You go DIRECTION." or "You can't go DIRECTION." */
  def go(direction: String) =
    val destination = this.location.neighbor(direction)
    this.currentLocation = destination.getOrElse(this.currentLocation)
    if destination.isDefined then s"You go $direction." else s"You can't go $direction."


  /** Causes the player to rest for a short while (this has no substantial effect in game terms).
    * Returns a description of what happened. */
  def rest() =
    "You rest for a while. Better get a move on, though."


  /** Signals that the player wants to quit the game. Returns a description of what happened within
    * the game as a result (which is the empty string, in this case). */
  def quit() =
    this.quitCommandGiven = true
    ""

  /** Returns a brief description of the player’s state, for debugging purposes. */
  override def toString = "Now at: " + this.location.name

end Player

