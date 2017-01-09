/* * This is an open source non-commercial project. Dear PVS-Studio, please check it.
 * PVS-Studio Static Code Analyzer for C, C++ and C#: http://www.viva64.com
 */

#pragma once
//
// Various algorithm and other stuff which did not deserve its own file yet
//

namespace e4std {

    template<class T> struct remove_reference       { typedef T type; };
    template<class T> struct remove_reference<T &>  { typedef T type; };
    template<class T> struct remove_reference<T &&> { typedef T type; };

    // This somehow doesn't really work
    template<typename T>
    inline typename remove_reference<T>::type&& move(T&& arg) {
        return static_cast<typename remove_reference<T>::type&&>(arg);
    }

    // Copy stuff algorithm
    template<class InputIt, class OutputIt>
    OutputIt copy(InputIt first, InputIt last, OutputIt d_first) {
        while (first != last) {
            *d_first++ = *first++;
        }
        return d_first;
    }

    // Copy stuff if predicate is true algorithm
    template<class InputIt, class OutputIt, class UnaryPredicate>
    OutputIt copy_if(InputIt first, InputIt last,
                     OutputIt d_first, UnaryPredicate pred) {
        while (first != last) {
            if (pred(*first)) {
                *d_first++ = *first;
            }
            first++;
        }
        return d_first;
    }

    // Simple max(a,b) http://en.cppreference.com/w/cpp/algorithm/max
    template<class T>
    const T& max(const T& a, const T& b) {
        return (a < b) ? b : a;
    }

    template<class ForwardIt, class T>
    void fill_forward(ForwardIt first, ForwardIt last, const T& value) {
        for (; first != last; ++first) {
            *first = value;
        }
    }
    template<class ReverseIt, class T>
    void fill_backward(ReverseIt first, ReverseIt last, const T& value) {
        for (; first != last; --first) {
            *first = value;
        }
    }
    template<class It, class T>
    void fill(It first, It last, const T& value) {
        if (last > first) { fill_forward(first, last, value); }
        else { fill_backward(first, last, value); }
    };

    template <class ValueType>
    void move_objects(ValueType *first, ValueType* last, ValueType* dst) {
        for (; first != last; ++first, ++dst) {
            *dst = static_cast<ValueType&&>(*first);
//            *dst = move(*first);
        }
    }

    class RuntimeError {
    private:
        const char *err_ = nullptr;
    public:
        explicit RuntimeError(const char *e): err_(e) {}
        RuntimeError(const RuntimeError &) = default;
        virtual ~RuntimeError();
        virtual const char *what() const noexcept { return err_; }
    };

} // ns e4std
