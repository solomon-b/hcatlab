{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE InstanceSigs #-}
module Data.BinaryTree where


data BinaryTree a = Leaf a | Branch (BinaryTree a) (BinaryTree a)
