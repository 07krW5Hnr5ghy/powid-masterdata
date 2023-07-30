package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.CategoryDTO;
import com.proyect.masterdata.dto.MasterListDTO;
import com.proyect.masterdata.dto.request.RequestCategory;
import com.proyect.masterdata.dto.request.RequestMasterList;
import com.proyect.masterdata.dto.response.ResponseMasterList;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.handler.ErrorResponse;
import com.proyect.masterdata.services.ICategory;
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
@RequestMapping("/category")
@AllArgsConstructor
public class CategoryController {
    private final ICategory iCategory;

    @Operation(summary = "Lista las categorias",
            description = "Lista las categorias")
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
    public ResponseEntity<List<CategoryDTO>> listCategories() throws BadRequestExceptions {
        List<CategoryDTO> result = iCategory.listRecords();
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @Operation(summary = "Registra categoria",
            description = "Registra categoria")
    @PostMapping()
    public ResponseEntity<ResponseMasterList> addCategory(@RequestParam("name") String name,@RequestParam("description") String description) throws BadRequestExceptions{
        ResponseMasterList result = iCategory.addRecord(name,description);
        return new ResponseEntity<>(result,HttpStatus.OK);
    }

    @Operation(summary = "Eliminar categoria",
            description = "Eliminar categoria")
    @DeleteMapping()
    public ResponseEntity<ResponseMasterList> deleteCategory(@RequestParam("id") Long id) throws BadRequestExceptions{
        ResponseMasterList result = iCategory.deleteRecord(id);
        return new ResponseEntity<>(result,HttpStatus.OK);
    }

    @Operation(summary = "Editar categoria",
            description = "Editar categoria")
    @PutMapping()
    public ResponseEntity<CategoryDTO> updateCategory(@RequestBody RequestCategory data) throws BadRequestExceptions{
        CategoryDTO result = iCategory.updateRecord(data.getName(), data.getId(),data.getDescription());
        return new ResponseEntity<>(result,HttpStatus.OK);
    }
}
