package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.ShipmentItemDTO;
import com.proyect.masterdata.dto.request.RequestShipmentItem;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.IShipment;
import lombok.AllArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@CrossOrigin({ "*" })
@RequestMapping("shipment")
@AllArgsConstructor
public class ShipmentController {

    private final IShipment iShipment;

    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<ResponseSuccess> save(
            @RequestParam("serial") String serial,
            @RequestParam("warehouse") String warehouse,
            @RequestBody() List<RequestShipmentItem> requestShipmentItemList,
            @RequestParam("tokenUser") String tokenUser) throws BadRequestExceptions {
        ResponseSuccess result = iShipment.save(serial, warehouse, requestShipmentItemList, tokenUser);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

}
