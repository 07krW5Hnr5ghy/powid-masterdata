package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.*;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

import java.util.Date;
import java.util.List;
import java.util.concurrent.CompletableFuture;

public interface IStats {
    CompletableFuture<StatsCardDTO> listCardStats(
            Date updateStartDate,
            Date updateEndDate,
            String orderState,
            String user) throws BadRequestExceptions,InternalErrorExceptions;
    CompletableFuture<List<DailySaleSummaryDTO>> listDailySales(
            Date registrationStartDate,
            Date registrationEndDate,
            String user
    ) throws BadRequestExceptions, InternalErrorExceptions;
    CompletableFuture<List<DailySaleSummaryDTO>> listDailySalesByStatus(
            Date registrationStartDate,
            Date registrationEndDate,
            String status,
            String user
    ) throws BadRequestExceptions, InternalErrorExceptions;
    CompletableFuture<List<SellerSalesDTO>> listSellerSales(
            Date registrationStartDate,
            Date registrationEndDate,
            String user
    ) throws BadRequestExceptions,InternalErrorExceptions;
    CompletableFuture<List<SalesBrandDTO>> listSalesBrand(
            Date registrationStartDate,
            Date registrationEndDate,
            String user
    ) throws BadRequestExceptions,InternalErrorExceptions;
}
