package com.proyect.masterdata.services;

import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

import java.io.ByteArrayInputStream;
import java.util.concurrent.CompletableFuture;

public interface ITemplate {
    CompletableFuture<ByteArrayInputStream> purchase(Integer quantity,String supplier,String username) throws BadRequestExceptions,InternalErrorExceptions;
    CompletableFuture<ByteArrayInputStream> stockTransfer(Integer quantity,String warehouseName,String username) throws BadRequestExceptions,InternalErrorExceptions;
    CompletableFuture<ByteArrayInputStream> stockReturn(Integer quantity,String purchaseSerial,String username) throws BadRequestExceptions,InternalErrorExceptions;
    CompletableFuture<ByteArrayInputStream> stockReplenishment(Long orderId,String username) throws BadRequestExceptions,InternalErrorExceptions;
    CompletableFuture<ByteArrayInputStream> orderStock(Long orderId,String username) throws BadRequestExceptions,InternalErrorExceptions;
    CompletableFuture<ByteArrayInputStream> orderReturn(Long orderId,String username) throws BadRequestExceptions,InternalErrorExceptions;
    CompletableFuture<ByteArrayInputStream> product(Integer quantity,String username) throws BadRequestExceptions,InternalErrorExceptions;
    CompletableFuture<ByteArrayInputStream> supplierProduct(Integer quantity,String username) throws BadRequestExceptions, InternalErrorExceptions;
}
