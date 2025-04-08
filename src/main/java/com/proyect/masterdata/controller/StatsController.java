package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.*;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.services.IStats;
import com.proyect.masterdata.services.IUtil;
import lombok.AllArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.text.ParseException;
import java.time.OffsetDateTime;
import java.util.Date;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

@RestController
@CrossOrigin({ "*" })
@RequestMapping("stats")
@AllArgsConstructor
public class StatsController {
    private final IStats iStats;
    private final IUtil iUtil;

    @GetMapping("card")
    ResponseEntity<StatsCardDTO> cardStatistics(
            @RequestParam(value = "registrationStartDate",required = false) String rStartDate,
            @RequestParam(value = "registrationEndDate",required = false) String rEndDate,
            @RequestParam(value = "orderState",required = false) String orderState,
            @RequestParam("user") String user
    ) throws InternalErrorExceptions, BadRequestExceptions, InterruptedException, ExecutionException, ParseException {
        OffsetDateTime registrationStartDate = iUtil.parseToOffsetDateTime(rStartDate,true);
        OffsetDateTime registrationEndDate = iUtil.parseToOffsetDateTime(rEndDate, false);
        CompletableFuture<StatsCardDTO> result = iStats.listCardStats(
                registrationStartDate,
                registrationEndDate,
                orderState,
                user
        );
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }

    @GetMapping("summary")
    ResponseEntity<List<DailySaleSummaryDTO>> dailySaleSummary(
            @RequestParam(value = "registrationStartDate",required = false) String rStartDate,
            @RequestParam(value = "registrationEndDate",required = false) String rEndDate,
            @RequestParam("user") String user
    ) throws BadRequestExceptions, InternalErrorExceptions, ExecutionException, InterruptedException {
        OffsetDateTime registrationStartDate = iUtil.parseToOffsetDateTime(rStartDate,true);
        OffsetDateTime registrationEndDate = iUtil.parseToOffsetDateTime(rEndDate, false);
        CompletableFuture<List<DailySaleSummaryDTO>> result = iStats.listDailySales(
                registrationStartDate,
                registrationEndDate,
                user
        );
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }

    @GetMapping("summary-state")
    ResponseEntity<List<DailySaleSummaryDTO>> dailySaleSummaryByState(
            @RequestParam(value = "registrationStartDate",required = false) String rStartDate,
            @RequestParam(value = "registrationEndDate",required = false) String rEndDate,
            @RequestParam("orderState") String state,
            @RequestParam("user") String user
    ) throws BadRequestExceptions, InternalErrorExceptions, ExecutionException, InterruptedException, ParseException {
        OffsetDateTime registrationStartDate = iUtil.parseToOffsetDateTime(rStartDate,true);
        OffsetDateTime registrationEndDate = iUtil.parseToOffsetDateTime(rEndDate, false);
        CompletableFuture<List<DailySaleSummaryDTO>> result = iStats.listDailySalesByStatus(
                registrationStartDate,
                registrationEndDate,
                state,
                user
        );
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }

    @GetMapping("seller")
    ResponseEntity<List<SellerSalesDTO>> dailySaleSummaryByState(
            @RequestParam(value = "registrationStartDate",required = false) String rStartDate,
            @RequestParam(value = "registrationEndDate",required = false) String rEndDate,
            @RequestParam("user") String user
    ) throws BadRequestExceptions, InternalErrorExceptions, ExecutionException, InterruptedException, ParseException {
        OffsetDateTime registrationStartDate = iUtil.parseToOffsetDateTime(rStartDate,true);
        OffsetDateTime registrationEndDate = iUtil.parseToOffsetDateTime(rEndDate, false);
        CompletableFuture<List<SellerSalesDTO>> result = iStats.listSellerSales(
                registrationStartDate,
                registrationEndDate,
                user
        );
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }

    @GetMapping("brand")
    ResponseEntity<List<SalesBrandDTO>> dailySaleSummaryByBrand(
            @RequestParam(value = "registrationStartDate",required = false) String rStartDate,
            @RequestParam(value = "registrationEndDate",required = false) String rEndDate,
            @RequestParam("user") String user
    ) throws BadRequestExceptions, InternalErrorExceptions, ExecutionException, InterruptedException{
        OffsetDateTime registrationStartDate = iUtil.parseToOffsetDateTime(rStartDate,true);
        OffsetDateTime registrationEndDate = iUtil.parseToOffsetDateTime(rEndDate, false);
        CompletableFuture<List<SalesBrandDTO>> result = iStats.listSalesBrand(
                registrationStartDate,
                registrationEndDate,
                user
        );
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }

    @GetMapping("status")
    ResponseEntity<List<SalesStatusDTO>> saleSummaryStatus(
            @RequestParam(value = "registrationStartDate",required = false) String rStartDate,
            @RequestParam(value = "registrationEndDate",required = false) String rEndDate,
            @RequestParam("user") String user
    ) throws BadRequestExceptions, InternalErrorExceptions, ExecutionException, InterruptedException{
        OffsetDateTime registrationStartDate = iUtil.parseToOffsetDateTime(rStartDate,true);
        OffsetDateTime registrationEndDate = iUtil.parseToOffsetDateTime(rEndDate, false);
        CompletableFuture<List<SalesStatusDTO>> result = iStats.listSalesStatus(
                registrationStartDate,
                registrationEndDate,
                user
        );
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }

    @GetMapping("channel")
    ResponseEntity<List<SalesChannelDTO>> saleSummaryChannel(
            @RequestParam(value = "registrationStartDate",required = false) String rStartDate,
            @RequestParam(value = "registrationEndDate",required = false) String rEndDate,
            @RequestParam("user") String user
    ) throws BadRequestExceptions, InternalErrorExceptions, ExecutionException, InterruptedException{
        OffsetDateTime registrationStartDate = iUtil.parseToOffsetDateTime(rStartDate,true);
        OffsetDateTime registrationEndDate = iUtil.parseToOffsetDateTime(rEndDate, false);
        CompletableFuture<List<SalesChannelDTO>> result = iStats.listSalesChannel(
                registrationStartDate,
                registrationEndDate,
                user
        );
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }

    @GetMapping("category")
    ResponseEntity<Page<SalesCategoryDTO>> saleSummaryCategory(
            @RequestParam(value = "registrationStartDate",required = false) String rStartDate,
            @RequestParam(value = "registrationEndDate",required = false) String rEndDate,
            @RequestParam("user") String user,
            @RequestParam(defaultValue = "0") Integer page,
            @RequestParam(defaultValue = "10") Integer size
    ) throws BadRequestExceptions, InternalErrorExceptions, ExecutionException, InterruptedException{
        OffsetDateTime registrationStartDate = iUtil.parseToOffsetDateTime(rStartDate,true);
        OffsetDateTime registrationEndDate = iUtil.parseToOffsetDateTime(rEndDate, false);
        CompletableFuture<Page<SalesCategoryDTO>> result = iStats.listCategories(
                registrationStartDate,
                registrationEndDate,
                user,
                page,
                size
        );
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }
}
