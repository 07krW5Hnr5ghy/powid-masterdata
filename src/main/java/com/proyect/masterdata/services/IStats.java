package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.*;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import org.springframework.data.domain.Page;

import java.util.Date;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;

public interface IStats {
    CompletableFuture<StatsCardDTO> listCardStats(
            UUID updateStartDate,
            UUID updateEndDate,
            String orderState,
            String user) throws BadRequestExceptions,InternalErrorExceptions;
    CompletableFuture<List<DailySaleSummaryDTO>> listDailySales(
            UUID registrationStartDate,
            UUID registrationEndDate,
            String user
    ) throws BadRequestExceptions, InternalErrorExceptions;
    CompletableFuture<List<DailySaleSummaryDTO>> listDailySalesByStatus(
            UUID registrationStartDate,
            UUID registrationEndDate,
            String status,
            String user
    ) throws BadRequestExceptions, InternalErrorExceptions;
    CompletableFuture<List<SellerSalesDTO>> listSellerSales(
            UUID registrationStartDate,
            UUID registrationEndDate,
            String user
    ) throws BadRequestExceptions,InternalErrorExceptions;
    CompletableFuture<List<SalesBrandDTO>> listSalesBrand(
            UUID registrationStartDate,
            UUID registrationEndDate,
            String user
    ) throws BadRequestExceptions,InternalErrorExceptions;
    CompletableFuture<List<SalesStatusDTO>> listSalesStatus(
            UUID registrationStartDate,
            UUID registrationEndDate,
            String user
    ) throws BadRequestExceptions,InternalErrorExceptions;
    CompletableFuture<List<SalesChannelDTO>> listSalesChannel(
            UUID registrationStartDate,
            UUID registrationEndDate,
            String user
    ) throws BadRequestExceptions,InternalErrorExceptions;
    CompletableFuture<Page<SalesCategoryDTO>> listCategories(
            UUID registrationStartDate,
            UUID registrationEndDate,
            String user,
            Integer page,
            Integer size
    ) throws BadRequestExceptions,InternalErrorExceptions;
}
