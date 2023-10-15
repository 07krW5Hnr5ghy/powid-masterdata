package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.PaymentDTO;
import com.proyect.masterdata.dto.request.RequestPaymentSave;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.IPayment;
import lombok.AllArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@CrossOrigin({"*"})
@RequestMapping("/payment")
@AllArgsConstructor
public class PaymentController {

    private final IPayment iPayment;
    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<ResponseSuccess> save(
            @RequestBody()RequestPaymentSave requestPaymentSave,
            @RequestParam("user") String user
    ) throws BadRequestExceptions {
        ResponseSuccess result = iPayment.save(requestPaymentSave,user);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @PostMapping(value = "/payments",consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<ResponseSuccess> saveAll(
            @RequestBody() List<RequestPaymentSave> requestPaymentSaveList,
            @RequestParam("user") String user
    ) throws BadRequestExceptions {
        ResponseSuccess result = iPayment.saveAll(requestPaymentSaveList,user);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<Page<PaymentDTO>> list(
            @RequestParam(value = "totalPayment",required = false) Double totalPayment,
            @RequestParam(value = "month",required = false) String month,
            @RequestParam(value = "channel",required = false) String channel,
            @RequestParam(value = "sort",required = false) String sort,
            @RequestParam(value = "sortColumn",required = false) String sortColumn,
            @RequestParam("pageNumber") Integer pageNumber,
            @RequestParam("pageSize") Integer pageSize
    ) throws BadRequestExceptions{
        Page<PaymentDTO> result = iPayment.list(totalPayment,month,channel,sort,sortColumn,pageNumber,pageSize);
        return new ResponseEntity<>(result,HttpStatus.OK);
    }

}
