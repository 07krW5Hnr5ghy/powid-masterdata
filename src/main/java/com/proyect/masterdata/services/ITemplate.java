package com.proyect.masterdata.services;

import com.proyect.masterdata.exceptions.BadRequestExceptions;

import java.io.ByteArrayInputStream;
import java.util.concurrent.CompletableFuture;

public interface ITemplate {
    CompletableFuture<ByteArrayInputStream> purchase(Integer quantity, String username) throws BadRequestExceptions;
    CompletableFuture<ByteArrayInputStream> shipment(Integer quantity, String purchaseSerial,String username) throws BadRequestExceptions;
    CompletableFuture<ByteArrayInputStream> stockTransfer(Integer quantity,String warehouseName,String username) throws BadRequestExceptions;
    CompletableFuture<ByteArrayInputStream> stockReturn(Integer quantity,String purchaseSerial,String username) throws BadRequestExceptions;
    CompletableFuture<ByteArrayInputStream> stockReplenishment(Long orderId,String username) throws BadRequestExceptions;
}
