package com.proyect.masterdata.services;

import com.proyect.masterdata.exceptions.BadRequestExceptions;

import java.io.ByteArrayInputStream;
import java.util.concurrent.CompletableFuture;

public interface ITemplate {
    CompletableFuture<ByteArrayInputStream> createPurchase(Integer quantity, String tokenUser) throws BadRequestExceptions;
}
