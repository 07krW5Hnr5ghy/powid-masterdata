package com.proyect.masterdata.controller;


import com.proyect.masterdata.domain.OrderLog;
import com.proyect.masterdata.dto.OrderLogDTO;
import com.proyect.masterdata.dto.response.ResponseSuccess;
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

    @PostMapping("comment")
    public ResponseEntity<ResponseSuccess> addComment(
            @RequestParam("tokenUser") String tokenUser,
            @RequestParam("orderId") UUID orderId,
            @RequestParam("comment") String comment
    ) throws ExecutionException, InterruptedException {
        CompletableFuture<ResponseSuccess> result = iOrderLog.saveCommentUserAsync(
                tokenUser,
                orderId,
                comment
        );
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }

}
