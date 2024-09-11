package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.DailySaleSummaryDTO;
import com.proyect.masterdata.dto.StatsCardDTO;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.services.IStats;
import lombok.AllArgsConstructor;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

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

    @GetMapping("card")
    ResponseEntity<StatsCardDTO> cardStatistics(
            @RequestParam("updateDateStart") @DateTimeFormat(iso=DateTimeFormat.ISO.DATE) Date updateDateStart,
            @RequestParam("updateDateEnd") @DateTimeFormat(iso=DateTimeFormat.ISO.DATE) Date updateDateEnd,
            @RequestParam(value = "orderState",required = false) String orderState,
            @RequestParam("user") String user
    ) throws InternalErrorExceptions, BadRequestExceptions, InterruptedException, ExecutionException {
        CompletableFuture<StatsCardDTO> result = iStats.listCardStats(
                updateDateStart,
                updateDateEnd,
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
}
