package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.*;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.services.IStats;
import com.proyect.masterdata.services.IUtil;
import lombok.AllArgsConstructor;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.text.ParseException;
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
            @RequestParam("registrationDateStart") @DateTimeFormat(iso=DateTimeFormat.ISO.DATE) Date registrationDateStart,
            @RequestParam("registrationDateEnd") @DateTimeFormat(iso=DateTimeFormat.ISO.DATE) Date registrationDateEnd,
            @RequestParam(value = "orderState",required = false) String orderState,
            @RequestParam("user") String user
    ) throws InternalErrorExceptions, BadRequestExceptions, InterruptedException, ExecutionException, ParseException {
        CompletableFuture<StatsCardDTO> result = iStats.listCardStats(
                registrationDateStart,
                registrationDateEnd,
                orderState,
                user
        );
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }

    @GetMapping("summary")
    ResponseEntity<List<DailySaleSummaryDTO>> dailySaleSummary(
            @RequestParam("registrationDateStart") @DateTimeFormat(iso=DateTimeFormat.ISO.DATE) Date registrationDateStart,
            @RequestParam("registrationDateEnd") @DateTimeFormat(iso=DateTimeFormat.ISO.DATE) Date registrationDateEnd,
            @RequestParam("user") String user
    ) throws BadRequestExceptions, InternalErrorExceptions, ExecutionException, InterruptedException {
        CompletableFuture<List<DailySaleSummaryDTO>> result = iStats.listDailySales(
                registrationDateStart,
                registrationDateEnd,
                user
        );
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }

    @GetMapping("summary-state")
    ResponseEntity<List<DailySaleSummaryDTO>> dailySaleSummaryByState(
            @RequestParam("registrationDateStart") @DateTimeFormat(iso=DateTimeFormat.ISO.DATE) Date registrationDateStart,
            @RequestParam("registrationDateEnd") @DateTimeFormat(iso=DateTimeFormat.ISO.DATE) Date registrationDateEnd,
            @RequestParam("orderState") String state,
            @RequestParam("user") String user
    ) throws BadRequestExceptions, InternalErrorExceptions, ExecutionException, InterruptedException, ParseException {
        CompletableFuture<List<DailySaleSummaryDTO>> result = iStats.listDailySalesByStatus(
                registrationDateStart,
                registrationDateEnd,
                state,
                user
        );
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }

    @GetMapping("seller")
    ResponseEntity<List<SellerSalesDTO>> dailySaleSummaryByState(
            @RequestParam("registrationDateStart") @DateTimeFormat(iso=DateTimeFormat.ISO.DATE) Date registrationDateStart,
            @RequestParam("registrationDateEnd") @DateTimeFormat(iso=DateTimeFormat.ISO.DATE) Date registrationDateEnd,
            @RequestParam("user") String user
    ) throws BadRequestExceptions, InternalErrorExceptions, ExecutionException, InterruptedException, ParseException {
        CompletableFuture<List<SellerSalesDTO>> result = iStats.listSellerSales(
                registrationDateStart,
                registrationDateEnd,
                user
        );
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }

    @GetMapping("brand")
    ResponseEntity<List<SalesBrandDTO>> dailySaleSummaryByBrand(
            @RequestParam("registrationDateStart") @DateTimeFormat(iso=DateTimeFormat.ISO.DATE) Date registrationDateStart,
            @RequestParam("registrationDateEnd") @DateTimeFormat(iso=DateTimeFormat.ISO.DATE) Date registrationDateEnd,
            @RequestParam("user") String user
    ) throws BadRequestExceptions, InternalErrorExceptions, ExecutionException, InterruptedException{
        CompletableFuture<List<SalesBrandDTO>> result = iStats.listSalesBrand(
                registrationDateStart,
                registrationDateEnd,
                user
        );
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }

    @GetMapping("status")
    ResponseEntity<List<SalesStatusDTO>> saleSummaryStatus(
            @RequestParam("registrationDateStart") @DateTimeFormat(iso=DateTimeFormat.ISO.DATE) Date registrationDateStart,
            @RequestParam("registrationDateEnd") @DateTimeFormat(iso=DateTimeFormat.ISO.DATE) Date registrationDateEnd,
            @RequestParam("user") String user
    ) throws BadRequestExceptions, InternalErrorExceptions, ExecutionException, InterruptedException{
        CompletableFuture<List<SalesStatusDTO>> result = iStats.listSalesStatus(
                registrationDateStart,
                registrationDateEnd,
                user
        );
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }

    @GetMapping("channel")
    ResponseEntity<List<SalesChannelDTO>> saleSummaryChannel(
            @RequestParam("registrationDateStart") @DateTimeFormat(iso=DateTimeFormat.ISO.DATE) Date registrationDateStart,
            @RequestParam("registrationDateEnd") @DateTimeFormat(iso=DateTimeFormat.ISO.DATE) Date registrationDateEnd,
            @RequestParam("user") String user
    ) throws BadRequestExceptions, InternalErrorExceptions, ExecutionException, InterruptedException{
        CompletableFuture<List<SalesChannelDTO>> result = iStats.listSalesChannel(
                registrationDateStart,
                registrationDateEnd,
                user
        );
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }

    @GetMapping("category")
    ResponseEntity<List<SalesCategoryDTO>> saleSummaryCategory(
            @RequestParam("registrationDateStart") @DateTimeFormat(iso=DateTimeFormat.ISO.DATE) Date registrationDateStart,
            @RequestParam("registrationDateEnd") @DateTimeFormat(iso=DateTimeFormat.ISO.DATE) Date registrationDateEnd,
            @RequestParam("user") String user
    ) throws BadRequestExceptions, InternalErrorExceptions, ExecutionException, InterruptedException{
        CompletableFuture<List<SalesCategoryDTO>> result = iStats.listCategories(
                registrationDateStart,
                registrationDateEnd,
                user
        );
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }
}
