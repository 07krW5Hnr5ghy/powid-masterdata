package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.CourierPicture;
import com.proyect.masterdata.domain.OrderPaymentReceipt;
import com.proyect.masterdata.domain.Ordering;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.CourierPictureRepository;
import com.proyect.masterdata.repository.OrderingRepository;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.ICourierPicture;
import com.proyect.masterdata.services.IFile;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.List;

@Service
@RequiredArgsConstructor
@Log4j2
public class CourierPictureImpl implements ICourierPicture {
    private final UserRepository userRepository;
    private final OrderingRepository orderingRepository;
    private final CourierPictureRepository courierPictureRepository;
    private final IFile iFile;
    @Override
    public List<String> uploadPicture(List<MultipartFile> pictures, Long orderId, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        User user;
        Ordering ordering;
        List<String> picturesUrlList = new ArrayList<>();
        try{
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            ordering = orderingRepository.findById(orderId).orElse(null);
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if(user == null){
            throw new BadRequestExceptions(Constants.ErrorUser);
        }

        if(ordering == null){
            throw new InternalErrorExceptions(Constants.ErrorOrdering);
        }

        try{
            String folder = (user.getClient().getBusiness() + "_PEDIDOS").replace(" ","_");
            Date currentDate = new Date(System.currentTimeMillis());
            SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd HH-mm-ss");
            String dateString = dateFormat.format(currentDate);
            String formattedString = dateString.replace(" ", "_");
            String filename = "PEDIDO_" + orderId.toString() + "_" + user.getUsername() + "_" + formattedString;
            String folderPath = folder + "/" + filename;
            int pictureNumber = 1;
            if(pictures.isEmpty()){
                return Collections.emptyList();
            }
            for(MultipartFile receipt : pictures){
                String url = iFile.uploadFile(receipt,folderPath + "_COURIER_" + Integer.toString(pictureNumber));
                courierPictureRepository.save(CourierPicture.builder()
                        .pictureUrl(url)
                        .client(user.getClient())
                        .clientId(user.getClientId())
                        .ordering(ordering)
                        .orderId(ordering.getId())
                        .registrationDate(currentDate)
                        .tokenUser(user.getUsername())
                        .build());
                picturesUrlList.add(url);
                pictureNumber++;
            }
            return picturesUrlList;
        }catch (RuntimeException | IOException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }
}
