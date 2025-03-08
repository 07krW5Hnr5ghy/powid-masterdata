package com.proyect.masterdata.services;

import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

import java.io.ByteArrayInputStream;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;

public interface ITemplate {
    CompletableFuture<ByteArrayInputStream> brand(String brand,String username) throws BadRequestExceptions,InternalErrorExceptions;
    CompletableFuture<ByteArrayInputStream> purchase(String supplier,String username) throws BadRequestExceptions;
    CompletableFuture<ByteArrayInputStream> stockTransfer(Integer quantity,String warehouseName,String username) throws BadRequestExceptions;
    CompletableFuture<ByteArrayInputStream> stockReturn(String warehouse,String supplier,String username) throws BadRequestExceptions;
    CompletableFuture<ByteArrayInputStream> stockReplenishment(UUID orderId, String username) throws BadRequestExceptions;
    CompletableFuture<ByteArrayInputStream> orderStock(UUID orderId,String username) throws BadRequestExceptions;
    CompletableFuture<ByteArrayInputStream> orderReturn(UUID orderId,String username) throws BadRequestExceptions;
    CompletableFuture<ByteArrayInputStream> product(Integer quantity,String username) throws BadRequestExceptions;
    CompletableFuture<ByteArrayInputStream> supplierProduct(Integer quantity,String username) throws BadRequestExceptions;
    CompletableFuture<ByteArrayInputStream> model(Integer quantity,String username) throws BadRequestExceptions, InternalErrorExceptions;
}
