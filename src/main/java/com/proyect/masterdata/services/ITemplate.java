package com.proyect.masterdata.services;

import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

import java.io.ByteArrayInputStream;
import java.util.concurrent.CompletableFuture;

public interface ITemplate {
    CompletableFuture<ByteArrayInputStream> purchase(String supplier,String username) throws BadRequestExceptions;
    CompletableFuture<ByteArrayInputStream> stockTransfer(Integer quantity,String warehouseName,String username) throws BadRequestExceptions;
    CompletableFuture<ByteArrayInputStream> stockReturn(String warehouse,String supplier,String username) throws BadRequestExceptions;
    CompletableFuture<ByteArrayInputStream> stockReplenishment(Long orderId,String username) throws BadRequestExceptions;
    CompletableFuture<ByteArrayInputStream> orderStock(Long orderId,String username) throws BadRequestExceptions;
    CompletableFuture<ByteArrayInputStream> orderReturn(Long orderId,String username) throws BadRequestExceptions;
    CompletableFuture<ByteArrayInputStream> product(Integer quantity,String username) throws BadRequestExceptions;
    CompletableFuture<ByteArrayInputStream> supplierProduct(Integer quantity,String username) throws BadRequestExceptions;
    CompletableFuture<ByteArrayInputStream> model(Integer quantity,String username) throws BadRequestExceptions, InternalErrorExceptions;
}
