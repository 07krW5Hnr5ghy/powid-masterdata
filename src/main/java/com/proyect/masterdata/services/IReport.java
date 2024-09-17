package com.proyect.masterdata.services;

import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import org.springframework.stereotype.Repository;

import java.io.ByteArrayInputStream;
import java.util.Date;
import java.util.concurrent.CompletableFuture;

@Repository
public interface IReport {
    CompletableFuture<ByteArrayInputStream> generalStockReport(String username) throws BadRequestExceptions, InternalErrorExceptions;
    CompletableFuture<ByteArrayInputStream> warehouseStockReport(String username) throws BadRequestExceptions, InternalErrorExceptions;
    CompletableFuture<ByteArrayInputStream> dailySalesSummary(Date registrationStartDate,
                                                              Date registrationEndDate,
                                                              String username) throws BadRequestExceptions,InternalErrorExceptions;
    CompletableFuture<ByteArrayInputStream> SalesBySellerSummary(Date registrationStartDate,
                                                              Date registrationEndDate,
                                                              String username) throws BadRequestExceptions,InternalErrorExceptions;
}
