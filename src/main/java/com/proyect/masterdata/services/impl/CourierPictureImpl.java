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
import java.io.InputStream;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

@Service
@RequiredArgsConstructor
@Log4j2
public class CourierPictureImpl implements ICourierPicture {
    private final UserRepository userRepository;
    private final OrderingRepository orderingRepository;
    private final CourierPictureRepository courierPictureRepository;
    private final IFile iFile;
    @Override
    public CompletableFuture<List<String>> uploadPicture(MultipartFile[] pictures, Long orderId, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
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
                if(pictures.length == 0){
                    return Collections.emptyList();
                }
                for(MultipartFile multipartFile : pictures){
                    InputStream inputStream = multipartFile.getInputStream();
                    byte[] buffer = new byte[inputStream.available()];
                    if (buffer.length == 0) {
                        System.out.println("Received an empty file: " + multipartFile.getOriginalFilename());
                    }
                    String url = iFile.uploadFile(multipartFile,folderPath + "_COURIER_" + Integer.toString(pictureNumber)).get();
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
            } catch (ExecutionException e) {
                throw new RuntimeException(e);
            } catch (InterruptedException e) {
                throw new RuntimeException(e);
            }
        });
    }
}
