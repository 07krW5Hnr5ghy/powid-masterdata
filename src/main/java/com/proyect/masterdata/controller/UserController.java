package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.UserDTO;
import com.proyect.masterdata.dto.UserQueryDTO;
import com.proyect.masterdata.dto.request.RequestUser;
import com.proyect.masterdata.dto.request.RequestUserSave;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.IUser;
import lombok.AllArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@CrossOrigin({"*"})
@RequestMapping("/user")
@AllArgsConstructor
public class UserController {
    private IUser iUser;
    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<ResponseSuccess> save(
            @RequestBody() RequestUser requestUser
            ) throws BadRequestExceptions {
        ResponseSuccess result = iUser.save(requestUser);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @PostMapping(value = "users",consumes=MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<ResponseSuccess> saveAll(
            @RequestBody() List<RequestUser> requestUserList,
            @RequestParam("user") String user
    ) throws BadRequestExceptions {
        ResponseSuccess result = iUser.saveAll(requestUserList,user);
        return new ResponseEntity<>(result,HttpStatus.OK);
    }

    @PutMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<UserDTO> update(
            @RequestBody() RequestUserSave requestUserSave,
            @RequestParam("user") String user
    ) throws BadRequestExceptions {
        UserDTO result = iUser.update(requestUserSave,user);
        return new ResponseEntity<>(result,HttpStatus.OK);
    }

    @DeleteMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<ResponseDelete> delete(
            @RequestParam("user") String user
    ) throws BadRequestExceptions{
        ResponseDelete result = iUser.delete(user);
        return new ResponseEntity<>(result,HttpStatus.OK);
    }

    @GetMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<Page<UserQueryDTO>> list(
            @RequestParam(value = "user",required = false) String user,
            @RequestParam(value = "status",required = false) Long status,
            @RequestParam(value = "sort",required = false) String sort,
            @RequestParam(value = "sortColumn",required = false) String sortColumn,
            @RequestParam("pageNumber") Integer pageNumber,
            @RequestParam("pageSize") Integer pageSize
    ) throws BadRequestExceptions{
        Page<UserQueryDTO> result = iUser.list(user,status,sort,sortColumn,pageNumber,pageSize);
        return new ResponseEntity<>(result,HttpStatus.OK);
    }
}
