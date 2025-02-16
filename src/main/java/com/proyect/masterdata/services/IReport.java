package com.proyect.masterdata.services;

import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import org.springframework.stereotype.Repository;

import java.io.ByteArrayInputStream;
import java.time.OffsetDateTime;
import java.util.concurrent.CompletableFuture;

@Repository
public interface IReport {
    CompletableFuture<ByteArrayInputStream> generalStockReport(String username) throws BadRequestExceptions, InternalErrorExceptions;
    CompletableFuture<ByteArrayInputStream> warehouseStockReport(String username) throws BadRequestExceptions, InternalErrorExceptions;
    CompletableFuture<ByteArrayInputStream> dailySalesSummary(OffsetDateTime registrationStartDate,
                                                              OffsetDateTime registrationEndDate,
                                                              String username) throws BadRequestExceptions,InternalErrorExceptions;
    CompletableFuture<ByteArrayInputStream> salesBySellerSummary(OffsetDateTime registrationStartDate,
                                                              OffsetDateTime registrationEndDate,
                                                              String username) throws BadRequestExceptions,InternalErrorExceptions;
    CompletableFuture<ByteArrayInputStream> salesByBrandSummary(OffsetDateTime registrationStartDate,
                                                                OffsetDateTime registrationEndDate,
                                                                String username) throws BadRequestExceptions,InternalErrorExceptions;
    CompletableFuture<ByteArrayInputStream> dailySalesByBrandSummary(OffsetDateTime registrationStartDate,
                                                                OffsetDateTime registrationEndDate,
                                                                String username) throws BadRequestExceptions,InternalErrorExceptions;
    CompletableFuture<ByteArrayInputStream> salesByStatusSummary(OffsetDateTime registrationStartDate,
                                                                     OffsetDateTime registrationEndDate,
                                                                     String username) throws BadRequestExceptions,InternalErrorExceptions;
    CompletableFuture<ByteArrayInputStream> salesByCategory(OffsetDateTime registrationStartDate,
                                                                 OffsetDateTime registrationEndDate,
                                                                 String username) throws BadRequestExceptions,InternalErrorExceptions;
}
