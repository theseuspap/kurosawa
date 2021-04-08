SELECT * FROM `orders`
   WHERE brand = 'EF_GR'
   AND vendor_id IN (
       SELECT id FROM `bookings`)
   AND day_key >= (SELECT MIN(DATE(created_at))
       FROM `bookings`)
