package com.proyect.masterdata.services;

import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

import java.io.InputStream;
import java.util.concurrent.CompletableFuture;

public interface IPdfGenerator {
    CompletableFuture<InputStream> generate() throws BadRequestExceptions, InternalErrorExceptions;
}
