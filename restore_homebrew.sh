#!/bin/bash

failed_items=""
function install_package() {
echo EXECUTING: brew install $1 $2
brew install $1 $2
[ $? -ne 0 ] && $failed_items="$failed_items $1"  # package failed to install.
}
brew tap caskroom/cask
brew tap caskroom/fonts
brew tap caskroom/versions
brew tap homebrew/games
brew tap homebrew/science
brew tap homebrew/versions
brew tap staticfloat/julia
install_package android-sdk ''
install_package arpack-julia ''
install_package arpack64-julia ''
install_package atk ''
install_package atkmm ''
install_package autoconf ''
install_package automake ''
install_package bazaar ''
install_package bdw-gc ''
install_package bfg ''
install_package bison ''
install_package boost ''
install_package brew-cask ''
install_package cabal-install ''
install_package cairo ''
install_package cairomm ''
install_package cloc ''
install_package cloog ''
install_package cloog018-julia ''
install_package cmake ''
install_package cscope ''
install_package elasticsearch ''
install_package fftw ''
install_package fontconfig ''
install_package freetype ''
install_package gcc ''
install_package gdbm ''
install_package gdk-pixbuf ''
install_package gettext ''
install_package ghc ''
install_package git ''
install_package git-latexdiff ''
install_package glib ''
install_package glibmm ''
install_package gmp ''
install_package gmp4-julia ''
install_package go ''
install_package gobject-introspection ''
install_package graphviz ''
install_package groovy ''
install_package gsl ''
install_package gstreamer ''
install_package gtk+ ''
install_package gtkmm ''
install_package harfbuzz ''
install_package hdf5 ''
install_package heroku-toolbelt ''
install_package hicolor-icon-theme ''
install_package hub ''
install_package hugo ''
install_package icu4c ''
install_package inkscape ''
install_package isl ''
install_package isl011-julia ''
install_package jpeg ''
install_package leiningen ''
install_package libevent ''
install_package libffi ''
install_package libmpc ''
install_package libpng ''
install_package libsigc++ ''
install_package libsodium ''
install_package libtiff ''
install_package libtool ''
install_package libxml2 ''
install_package libyaml ''
install_package little-cms ''
install_package llvm33-julia ''
install_package lua ''
install_package macvim '  --override-system-vim'
install_package mercurial ''
install_package mongodb ''
install_package mpfr ''
install_package mysql ''
install_package neovim ''
install_package ninvaders ''
install_package node ''
install_package ode ''
install_package open-mpi ''
install_package openblas ''
install_package openblas-julia ''
install_package openblas64-julia ''
install_package openssl ''
install_package ossp-uuid ''
install_package pandoc ''
install_package pango ''
install_package pangomm ''
install_package pcre ''
install_package pixman ''
install_package pkg-config ''
install_package popt ''
install_package postgresql ''
install_package pypy ''
install_package python ''
install_package r ''
install_package readline ''
install_package ruby ''
install_package rust ''
install_package sbt ''
install_package scala ''
install_package sdl ''
install_package sdl_mixer ''
install_package sdl_net ''
install_package sdl_ttf ''
install_package sqlite ''
install_package suite-sparse-julia ''
install_package suite-sparse64-julia ''
install_package szip ''
install_package tree ''
install_package valgrind ''
install_package vitetris ''
install_package zeromq ''
[ ! -z $failed_items ] && echo The following items were failed to install: && echo $failed_items
