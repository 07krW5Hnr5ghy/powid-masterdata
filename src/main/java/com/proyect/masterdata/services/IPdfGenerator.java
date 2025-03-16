package com.proyect.masterdata.services;

import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

import java.io.InputStream;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;

import com.itextpdf.layout.element.Table;

public interface IPdfGenerator {
    CompletableFuture<InputStream> generateOrderReport(UUID orderId, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions;
    CompletableFuture<InputStream> generateDeliveryManifestReport(String deliveryManifestId,String tokenUser) throws BadRequestExceptions,InternalErrorExceptions;
}
