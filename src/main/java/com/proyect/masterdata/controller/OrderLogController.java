package com.proyect.masterdata.controller;


import com.proyect.masterdata.domain.OrderLog;
import com.proyect.masterdata.dto.OrderLogDTO;
import com.proyect.masterdata.services.IOrderLog;
import lombok.AllArgsConstructor;
import org.springframework.data.repository.query.Param;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

@RestController
@CrossOrigin({"*"})
@RequestMapping("order-log")
@AllArgsConstructor
public class OrderLogController {
    private final IOrderLog iOrderLog;

    @GetMapping("list")
    public ResponseEntity<List<OrderLogDTO>> listOrderLogs(
            @RequestParam("orderId") UUID orderId
    ) throws ExecutionException, InterruptedException {
        //CompletableFuture<List<OrderLogDTO>> result = iOrderLog.listLogByOrder(orderId);
        //return new ResponseEntity<>(result.get(),HttpStatus.OK);
        return null;
    }

}
