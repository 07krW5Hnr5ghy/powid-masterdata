package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.MasterListDTO;
import com.proyect.masterdata.dto.response.ResponseMasterList;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.handler.ErrorResponse;
import com.proyect.masterdata.services.impl.SaleChannelImpl;
import com.proyect.masterdata.services.impl.SizeImpl;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import lombok.AllArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@CrossOrigin({"*"})
@RequestMapping("/sale-channel")
@AllArgsConstructor
public class SaleChannelController {
    private final SaleChannelImpl iSaleChannel;
    @Operation(summary = "Lista los canales de venta",
            description = "Lista los canales de venta")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Success",
                    content = { @Content(mediaType = "application/json", schema = @Schema(implementation = List.class))}),
            @ApiResponse(responseCode = "400", description = "Bad Request",
                    content = { @Content(mediaType = "application/json", schema = @Schema(implementation = ErrorResponse.class))}),
            @ApiResponse(responseCode = "401", description = "Unauthorized",
                    content = { @Content(mediaType = "application/json", schema = @Schema(implementation = ErrorResponse.class))}),
            @ApiResponse(responseCode = "403", description = "ForbiddenForbidden",
                    content = { @Content(mediaType = "application/json", schema = @Schema(implementation = ErrorResponse.class))}),
            @ApiResponse(responseCode = "404", description = "Not Found",
                    content = { @Content(mediaType = "application/json", schema = @Schema(implementation = ErrorResponse.class))}),
            @ApiResponse(responseCode = "409", description = "Conflict",
                    content = { @Content(mediaType = "application/json", schema = @Schema(implementation = ErrorResponse.class))}),
            @ApiResponse(responseCode = "500", description = "Internal Server Error",
                    content = { @Content(mediaType = "application/json", schema = @Schema(implementation = ErrorResponse.class))})
    })

    @GetMapping()
    public ResponseEntity<List<MasterListDTO>> listStates() throws BadRequestExceptions {
        List<MasterListDTO> result = iSaleChannel.listRecords();
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @Operation(summary = "Registra canal de venta",
            description = "Registra canal de venta")
    @PostMapping()
    public ResponseEntity<ResponseMasterList> addState(@RequestParam("name") String name) throws BadRequestExceptions{
        ResponseMasterList result = iSaleChannel.addRecord(name);
        return new ResponseEntity<>(result,HttpStatus.OK);
    }

    @Operation(summary = "Eliminar canal de venta",
            description = "Eliminar canal de venta")
    @DeleteMapping()
    public ResponseEntity<ResponseMasterList> deleteState(@RequestParam("id") Long id) throws BadRequestExceptions{
        ResponseMasterList result = iSaleChannel.deleteRecord(id);
        return new ResponseEntity<>(result,HttpStatus.OK);
    }

    @Operation(summary = "Editar canal de venta",
            description = "Editar canal de venta")
    @PutMapping()
    public ResponseEntity<MasterListDTO> updateState(@RequestBody MasterListDTO data) throws BadRequestExceptions{
        MasterListDTO result = iSaleChannel.updateRecord(data.getName(), data.getId());
        return new ResponseEntity<>(result,HttpStatus.OK);
    }
}
