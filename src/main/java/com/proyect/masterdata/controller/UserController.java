package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.UserDTO;
import com.proyect.masterdata.dto.request.RequestUserSave;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.mocks.UserMocks;
import com.proyect.masterdata.services.IUser;
import lombok.AllArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Arrays;
import java.util.List;

@RestController
@CrossOrigin({"*"})
@RequestMapping("/user")
@AllArgsConstructor
public class UserController {
    private IUser iUser;
    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<ResponseSuccess> save(
            @RequestBody() RequestUserSave requestUserSave
            ) throws BadRequestExceptions {
        ResponseSuccess result = iUser.save(requestUserSave);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @PostMapping(value = "users",consumes=MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<ResponseSuccess> saveAll(
            @RequestBody() List<RequestUserSave> requestUserSaveList,
            @RequestParam("user") String user
    ) throws BadRequestExceptions {
        ResponseSuccess result = iUser.saveAll(requestUserSaveList,user);
        return new ResponseEntity<>(result,HttpStatus.OK);
    }

    @GetMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<List<UserDTO>> listUsers(
            @RequestParam("user") String user
    ) throws BadRequestExceptions {
        UserMocks userMocks = new UserMocks();
        List<UserDTO> userList = Arrays.asList(userMocks.getUserList());
        return new ResponseEntity<>(userList, HttpStatus.OK);
    }

}
