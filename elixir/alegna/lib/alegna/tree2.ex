defmodule Alegna.Tree do
  @moduledoc """
  Implementation of a Merkle Tree on top of Full Binary Tree.

  Each node has 0 or 2 children. When adding nodes always 2 are added.

  The height indicates at which level in a tree a particular node is. We start
  counting from the bottom of a tree, i.e. from the leaves. The height of a leaf
  is 0. The root node is the highest one in a tree.

  The size of a tree indicates the number of the nodes it has.
  """

  alias __MODULE__

  @typedoc """
  The number of nodes in a Tree

  If a Tree is empty then it has 0 Nodes and its size is 0.
  """
  @type tsize :: non_neg_integer

  @typedoc """
  Either a Node or Leaf of a tree

  We can think of a Leaf as a sub-type of a Node, so that Leaf is also a Node,
  but a Node is not necessarily a Leaf.

  Leafs can only exist at the last level of a Tree.
  """
  @type tnode :: Node.t() | Leaf.t()

  @typedoc """
  An attribute of a node indicating at which level it is in the tree

  0 means a node is a Leaf.
  """
  @type theight :: non_neg_integer

  @typedoc """
  The hash of a node
  """
  @type thash :: String.t()

  defstruct size: 0, root: nil
  @type t :: %Tree{size: tsize, root: tnode}

  defmodule Leaf do
    @enforce_keys [:hash]
    defstruct height: 0, hash: nil
    @type t :: %Leaf{height: Tree.theight(), hash: String.t()}
  end

  defmodule Node do
    @enforce_keys [:height, :hash, :left, :right]
    defstruct [:height, :hash, :left, :right]

    @type t :: %Node{
            height: Tree.theight(),
            hash: Tree.thash(),
            left: Tree.tnode(),
            right: Tree.tnode()
          }
  end

  @doc "Creates an empty Tree"
  @spec new() :: t
  def new(), do: %Tree{}

  @doc """
  Adds a node to the `tree` for the given `value`

  Returns a tuple with an updated Tree and a Hash corresponding to the inserted
  `value`.
  """
  @spec add_node(t, term) :: {t, thash}
  # Binary tree always grows by 2, as it has only nodes with 0 or 2 children. The
  # only exception is when the tree is empty meaning that there are no nodes. Then
  # at the beginning just one node is added.
  def add_node(%Tree{size: 0, root: nil} = tree, value) do
    hash = hash(value)
    {%Tree{tree | size: 1, root: leaf(hash)}, hash}
  end

  def add_node(%Tree{size: s, root: r} = tree, value) do
    hash = hash(value)
    {%Tree{tree | size: s + 2, root: add_value(s, r, hash)}, hash}
  end

  defp add_node_on_top(left, right) do
    hash = concat_hashes(left.hash, right.hash)
    %Node{height: left.height + 1, hash: hash, left: left, right: right}
  end

  defp add_node_in_right_subtree(node, size, hash) do
    r_size = right_subtree_size(size, node.height)
    r_node = add_value(r_size, node.right, hash)
    n_hash = concat_hashes(node.left.hash, r_node.hash)
    %Node{node | hash: n_hash, right: r_node}
  end

  @spec add_value(tsize, node, thash) :: node
  defp add_value(size, %Node{} = node, hash) do
    if max_size?(size, node.height) do
      add_node_on_top(node, leaf(hash))
    else
      add_node_in_right_subtree(node, size, hash)
    end
  end

  defp add_value(_size, %Leaf{} = node, hash) do
    add_node_on_top(node, leaf(hash))
  end

  def empty?(%Tree{size: 0, root: nil}), do: true
  def empty?(_), do: false

  # INTERNALS

  defp leaf(hash), do: %Leaf{hash: hash}

  def hash(value), do: value

  defp concat_hashes(h1, h2), do: h1 <> h2

  @doc """
  True if the `size` represents the maximum number of nodes in a full binary
  tree of a given `height`
  """
  defp max_size?(size, height), do: size == max_size(height)

  @doc """
  Indicates the max number of nodes in a tree of the given `height`
  """
  defp max_size(height), do: :math.pow(2, height + 1) - 1

  @doc """
  Indicates the max number of nodes in a right subtree of the given tree

  Nodes are added in a fashion that the left subtree is filled first and it has
  always it's max size
  """
  defp right_subtree_size(size, height) do
    subtree_height = height - 1
    # from the current size subtract the size of the left subtree which is always
    # full and then subtract one for the root node
    size - max_size(subtree_height) - 1
  end
end
