// This is an open source non-commercial project. Dear PVS-Studio, please check it.
// PVS-Studio Static Code Analyzer for C, C++ and C#: http://www.viva64.com
//

#pragma once
//
// A custom, badly written replacement for C++ Map (using < for node ordering)
//

#include "e4std/free_fun.h"

namespace e4std {

template<class KeyType, class ValueType>
struct MapNode {
    KeyType key_;
    ValueType value_;
    MapNode* left_;
    MapNode* right_;

    explicit MapNode(const KeyType& k, const ValueType& v):
            key_(k), value_(v), left_(), right_() {}

    MapNode(const KeyType& k, const ValueType& v,
            MapNode* left, MapNode* right)
            : key_(k), value_(v), left_(left), right_(right) {}
};

template <class KeyType, class ValueType, class Allocator = platf::SingleAlloc>
class Map {
public:
    using NodeType = MapNode<KeyType, ValueType>;
    Map(): root_() {}

private:
    NodeType* root_;

public:
    void insert(const KeyType& key, const ValueType& val) {
        if (root_) {
            insert_helper(root_, key, val);
        } else {
            // TODO: use alloc class in g_platf/mem.h
            root_ = Allocator::template alloc_class<NodeType>(key, val);
        }
    }

    ::size_t size() const {
        return count_nodes_helper(root_);
    }

    ::size_t depth() const {
        return depth_helper(this->root_);
    }

    using VisitorFun = void (*)(const void* k, const void* v, void* extra);
    void visit_nodes(VisitorFun fn, void* extra) const {
        return visit_nodes_helper(root_, fn, extra);
    }


    bool remove(const KeyType& key) {
        return remove_helper(nullptr, root_, key);
    }

    NodeType* find(const KeyType& key) const {
        return find_helper(root_, key);
    }

private:
    void insert_helper(NodeType* root,
                       const KeyType& key, const ValueType& val) {
        if (compare_less(key, root->key_)) {
            if (not root->left_) {
                root->left_ = Allocator::template alloc_class<NodeType>(key, val);
            } else {
                insert_helper(root->left_, key, val);
            }
        } else {
            if (not root->right_) {
                root->right_ = Allocator::template alloc_class<NodeType>(key, val);
            } else {
                insert_helper(root->right_, key, val);
            }
        }
    }

    ::size_t count_nodes_helper(const NodeType* root) const {
        if (not root) { return 0; }
        else {
            return 1 + count_nodes_helper(root->left_) +
                   count_nodes_helper(root->right_);
        }
    }

    void visit_nodes_helper(const NodeType* root,
                            VisitorFun eachfn,
                            void* extra) const {
        if (not root) { return; }
        else {
            eachfn(&root->key_, &root->value_, extra);
            visit_nodes_helper(root->left_, eachfn, extra);
            visit_nodes_helper(root->right_, eachfn, extra);
        }
    }

    ::size_t depth_helper(const NodeType* root) const {
        if (not root) { return 0; }
        else {
            return 1 + max(depth_helper(root->left_),
                           depth_helper(root->right_));
        }
    }

    bool remove_helper(NodeType* parent, NodeType* current,
                       const KeyType& key) {
        if (not current) { return false; }
        if (compare_equal(current->key_, key)) {
            if (not current->left_ || not current->right_) {
                NodeType* t = current->left_;
                if (current->right_) { t = current->right_; }
                if (parent) {
                    if (parent->left_ == current) {
                        parent->left_ = t;
                    } else {
                        parent->right_ = t;
                    }
                } else {
                    this->root_ = t;
                }
            } else {
                NodeType* valid_sub = current->right_;
                while (valid_sub->left_) {
                    valid_sub = valid_sub->left_;
                }
                KeyType u = current->key_;
                current->key_ = valid_sub->key_;
                valid_sub->key_ = u;
                return remove_helper(current, current->right_, u);
            }
            Allocator::free(current);
            return true;
        }

        if (compare_less(key, current->key_)) {
            return remove_helper(current, current->left_, key);
        } else {
            return remove_helper(current, current->right_, key);
        }
    }

    NodeType* find_helper(NodeType* current, KeyType key) const {
        if (not current) { return nullptr; }
        if (compare_equal(current->key_, key)) { return current; }

        if (compare_less(key, current->key_)) {
            return find_helper(current->left_, key);
        } else {
            return find_helper(current->right_, key);
        }
    }
};

} // ns e4std
