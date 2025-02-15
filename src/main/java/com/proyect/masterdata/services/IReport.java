package com.proyect.masterdata.services;

import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import org.springframework.stereotype.Repository;

import java.io.ByteArrayInputStream;
import java.util.Date;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;

@Repository
public interface IReport {
    CompletableFuture<ByteArrayInputStream> generalStockReport(String username) throws BadRequestExceptions, InternalErrorExceptions;
    CompletableFuture<ByteArrayInputStream> warehouseStockReport(String username) throws BadRequestExceptions, InternalErrorExceptions;
    CompletableFuture<ByteArrayInputStream> dailySalesSummary(UUID registrationStartDate,
                                                              UUID registrationEndDate,
                                                              String username) throws BadRequestExceptions,InternalErrorExceptions;
    CompletableFuture<ByteArrayInputStream> salesBySellerSummary(UUID registrationStartDate,
                                                              UUID registrationEndDate,
                                                              String username) throws BadRequestExceptions,InternalErrorExceptions;
    CompletableFuture<ByteArrayInputStream> salesByBrandSummary(UUID registrationStartDate,
                                                                UUID registrationEndDate,
                                                                String username) throws BadRequestExceptions,InternalErrorExceptions;
    CompletableFuture<ByteArrayInputStream> dailySalesByBrandSummary(UUID registrationStartDate,
                                                                UUID registrationEndDate,
                                                                String username) throws BadRequestExceptions,InternalErrorExceptions;
    CompletableFuture<ByteArrayInputStream> salesByStatusSummary(UUID registrationStartDate,
                                                                     UUID registrationEndDate,
                                                                     String username) throws BadRequestExceptions,InternalErrorExceptions;
    CompletableFuture<ByteArrayInputStream> salesByCategory(UUID registrationStartDate,
                                                                 UUID registrationEndDate,
                                                                 String username) throws BadRequestExceptions,InternalErrorExceptions;
}
