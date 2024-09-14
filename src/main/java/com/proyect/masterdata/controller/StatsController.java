package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.DailySaleSummaryDTO;
import com.proyect.masterdata.dto.StatsCardDTO;
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
            @RequestParam("updateDateStart") @DateTimeFormat(iso=DateTimeFormat.ISO.DATE) Date updateDateStart,
            @RequestParam("updateDateEnd") @DateTimeFormat(iso=DateTimeFormat.ISO.DATE) Date updateDateEnd,
            @RequestParam(value = "orderState",required = false) String orderState,
            @RequestParam("user") String user
    ) throws InternalErrorExceptions, BadRequestExceptions, InterruptedException, ExecutionException, ParseException {
        Date utcUpdateDateStart = iUtil.setToUTCStartOfDay(updateDateStart);
        Date utcUpdateDateEnd = iUtil.setToUTCStartOfDay(updateDateEnd);
        System.out.println(utcUpdateDateStart);
        System.out.println(utcUpdateDateEnd);
        CompletableFuture<StatsCardDTO> result = iStats.listCardStats(
                utcUpdateDateStart,
                utcUpdateDateEnd,
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
        Date utcUpdateDateStart = iUtil.setToUTCStartOfDay(registrationDateStart);
        Date utcUpdateDateEnd = iUtil.setToUTCStartOfDay(registrationDateEnd);
        System.out.println(utcUpdateDateStart);
        System.out.println(utcUpdateDateEnd);
        CompletableFuture<List<DailySaleSummaryDTO>> result = iStats.listDailySalesByStatus(
                registrationDateStart,
                registrationDateEnd,
                state,
                user
        );
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }
}
