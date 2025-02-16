package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.*;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import org.springframework.data.domain.Page;

import java.time.OffsetDateTime;
import java.util.List;
import java.util.concurrent.CompletableFuture;

public interface IStats {
    CompletableFuture<StatsCardDTO> listCardStats(
            OffsetDateTime updateStartDate,
            OffsetDateTime updateEndDate,
            String orderState,
            String user) throws BadRequestExceptions,InternalErrorExceptions;
    CompletableFuture<List<DailySaleSummaryDTO>> listDailySales(
            OffsetDateTime registrationStartDate,
            OffsetDateTime registrationEndDate,
            String user
    ) throws BadRequestExceptions, InternalErrorExceptions;
    CompletableFuture<List<DailySaleSummaryDTO>> listDailySalesByStatus(
            OffsetDateTime registrationStartDate,
            OffsetDateTime registrationEndDate,
            String status,
            String user
    ) throws BadRequestExceptions, InternalErrorExceptions;
    CompletableFuture<List<SellerSalesDTO>> listSellerSales(
            OffsetDateTime registrationStartDate,
            OffsetDateTime registrationEndDate,
            String user
    ) throws BadRequestExceptions,InternalErrorExceptions;
    CompletableFuture<List<SalesBrandDTO>> listSalesBrand(
            OffsetDateTime registrationStartDate,
            OffsetDateTime registrationEndDate,
            String user
    ) throws BadRequestExceptions,InternalErrorExceptions;
    CompletableFuture<List<SalesStatusDTO>> listSalesStatus(
            OffsetDateTime registrationStartDate,
            OffsetDateTime registrationEndDate,
            String user
    ) throws BadRequestExceptions,InternalErrorExceptions;
    CompletableFuture<List<SalesChannelDTO>> listSalesChannel(
            OffsetDateTime registrationStartDate,
            OffsetDateTime registrationEndDate,
            String user
    ) throws BadRequestExceptions,InternalErrorExceptions;
    CompletableFuture<Page<SalesCategoryDTO>> listCategories(
            OffsetDateTime registrationStartDate,
            OffsetDateTime registrationEndDate,
            String user,
            Integer page,
            Integer size
    ) throws BadRequestExceptions,InternalErrorExceptions;
}
