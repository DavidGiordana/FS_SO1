import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.Socket;
import java.net.UnknownHostException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Hashtable;
import java.util.Random;

import org.apache.commons.io.FileUtils;
/**
 * Automatizador de mensajes a servidor de archivos
 * @author David Giordana
 *
 */
class Telex{

	//Cantidad máxima de clientes
	public static final int MAX_CLIENTS = 4;

	//Nombre del archivo de mensajes por defecto
	public static final String MSG_FILE = "msg.txt";

	//DAtos de conexion por defecto
	public static final String DEFAULT_IP = "127.0.0.1";
	public static final int DEFAULT_PORT = 8001;

	//Códigos de comandos válidos
	public static final int ERROR = -1;
	public static final int CON = 0;
	public static final int LSD = 1;
	public static final int CRE = 2;
	public static final int OPN = 3;
	public static final int CLO = 4;
	public static final int WRT = 5;
	public static final int REA = 6;
	public static final int BYE = 7;

	//Arreglo de comandos válidos
	public static final String[] COMANDS = {
			"CON",
			"LSD",
			"CRE",
			"OPN",
			"CLO",
			"WRT",
			"REA",
			"BYE"
	};

	//Posible nombres de archivos 
	public static final String[] possibleNames = {"aa", "bb", "cc", "dd", "ee", "ff", "gg", "hh", "ii", "jj", "kk", "ll", "mm","nn", "oo", "pp", "qq", "rr", "ss", "tt", "uu", "vv", "ww", "yy", "zz"};

	//Posibles contenidos de archivos
	public static final String[] possibleContent = {"uno", "dos", "sss", "qua", "cin", "las", "Fas", "Opa", "cas"};

	//Generador de numeros al azar
	private static Random rand = new Random();

	//Archivo que contiene las colas de mensajes de los usuarios
	private static File file = new File((new File(".")).getAbsolutePath() + File.separator + MSG_FILE);

	//Tabla de (usuario, linea)
	private static Hashtable<Integer, ArrayList<String>> table;

	//Datos para la conexion de internet
	private static String ip = DEFAULT_IP;
	private static int port = DEFAULT_PORT;

	//Cantidad de usuarios
	private static int cantusers = MAX_CLIENTS;

	//Vale true si los mensajes son al azar
	private static boolean isRandom = true;

	//Lista de clientes creados
	private static ArrayList<Thread> clients = new ArrayList<Thread>();

	/**
	 * Crea una linea de mensaje
	 * @param com Valor del comando 
	 * @param fd Descriptor de archivo, 0 indica que no se usa
	 * @param size Tamaño, 0 indica que no se usa
	 * @param str Cadena de texto, "" indica que no se usa
	 * @return mensaje a enviar
	 */
	public static String createInstruction(int com, int fd, int size, String str){
		String ret = COMANDS[com];
		if(fd > 0){
			ret += " FD " + fd;
		}
		if(size > 0){
			ret += " SIZE " + size;
		}
		if(str != ""){
			ret += " " + str;
		}
		return ret;
	}

	/**
	 * Agrega una instruccion a la tabla
	 * @param id Identificador de usuario
	 * @param line Linea que contiene la instrucción
	 */
	private static void addInstructionToTable(Integer id, String line){
		if(table.containsKey(id)){
			table.get(id).add(line);
		}
		else{
			ArrayList<String> l = new ArrayList<String>();
			l.add(line);
			table.put(id, l);
		}
	}

	/**
	 * Genera una instruccion al azar
	 * @return Instruccion generada
	 */
	public static String gemerateRandomInstruction(){
		int com = rand.nextInt(BYE - 1) + 1;
		int fd = rand.nextInt(possibleNames.length) + 1;
		int size = rand.nextInt(3) + 1;
		String str = possibleContent[rand.nextInt(possibleContent.length)].substring(0, size);
		if(com == CON || com == LSD || com == BYE){      
			return createInstruction(com, 0, 0, "");
		}
		else if(com == CRE){
			return createInstruction(com, 0, 0, str);
		}
		else if(com == OPN || com == CLO){  
			return createInstruction(com, fd, 0, "");
		}
		else if(com == WRT){    
			return createInstruction(com, fd, size, str);
		}
		else if(com == REA){
			return createInstruction(com, fd, size, "");
		}
		return null;
	}

	/**
	 * Parsea el archivo de mensajes
	 */
	@SuppressWarnings("unchecked")
	public static void parseFile(){
		ArrayList<String> lines;
		try{
			lines = new ArrayList<String>(FileUtils.readLines(file));
		}catch(IOException e){
			e.printStackTrace();
			return;
		}
		table = new Hashtable<Integer, ArrayList<String>>();
		for(String str : lines){
			int index = str.indexOf(" ");
			int client = Integer.parseInt(str.substring(0, index));
			String comand = str.substring(index + 1);
			addInstructionToTable(client, comand);
		}
	}

	/**
	 * Cliente para comunicarse con servidor de archivos 
	 * @author David Giordana
	 *
	 */
	public static class Client extends Thread{

		//Identificador de cliente
		private int id;

		//cola de mensajes a enviar
		private ArrayList<String> queue;

		//Socket para comunicarse con el servidor
		private Socket socket;

		//Entrada de datos
		private BufferedReader in;

		//Salida de datos
		private PrintWriter out;

		//Inica que se debe detener en envio de mensajes, usado en random
		private boolean stop;

		/**
		 * Constructor del cliente
		 * @param id Identidicador de cliente, debe ser un entero
		 * @param queue Pila de mensajes a enviar
		 * @throws UnknownHostException, IOException El constructor puede fallar al crear el socket
		 */
		public Client(int id, ArrayList<String> queue) throws UnknownHostException, IOException{
			this.socket = new Socket(ip, port);
			this.id = id;
			this.queue = queue;
			this.in = new BufferedReader(new InputStreamReader(socket.getInputStream()));
			this.out = new PrintWriter(socket.getOutputStream(), true);
		}

		@Override
		public void run(){
			if(isRandom){
				sendAndReceive(COMANDS[CON]);
				while(!stop){
					String line = gemerateRandomInstruction();
					stop = line.startsWith(COMANDS[BYE]);
					sendAndReceive(line);
				}   
			}
			else{
				while(!queue.isEmpty()){
					String line = queue.remove(0);
					sendAndReceive(line);
				}
			}
			clients.remove(this);
		}

		/**
		 * Imprime un mensaje
		 * @param str Mensaje a imprimir
		 * @param sending True si se está enviando un mensaje, False si se está recibiendo
		 */
		private void printMessage(String str, boolean sending){
			if(!sending && str == null){
				stop = true;
				return;
			}
			String temp = "Usuario \"" + this.id + "\" ";
			if(sending){
				temp += "envia: \"" + str + "\"";
			}
			else{
				temp += "recibe: \"" + str + "\"\n";
			}
			System.out.println(temp);
		}    

		/**
		 * Envia y recibe un mensaje al servidor
		 * @param str Mensaje a enviar
		 */
		private void sendAndReceive(String str){
			try{
				printMessage(str, true);				
				out.println(str + "  ");
				Thread.sleep(500);
				String read = in.readLine();
				printMessage(read, false);
			}catch(Exception e){
				//e.printStackTrace();
				System.out.println("Error al enviar o recibir la infornación");
				stop = true;
			}
		}
	}

	/**
	 * Parsea los argumentos de la linea de comando
	 * @param args Argumentos de la linea de comando
	 * @return	true si pudo parsear correctamente la información
	 */
	public static boolean parserCommandLineArgs(String[] args){
		//Variables de control
		boolean nextIp = false;
		boolean nextPort = false;
		boolean nextRandom = false;
		boolean nextFile = false;
		boolean ok = true;
		//Muestra la ayuda
		if(args.length == 1 && args[0] == "help"){

			return false;
		}
		for(String str: args){
			if(nextIp){
				ip = str;
				nextIp = false;
			}
			else if(nextPort){
				port = Integer.parseInt(str);
				nextPort = false;
			}
			else if(nextRandom){
				cantusers = Integer.parseInt(str);
				nextRandom = false;
			}
			else if(nextFile){
				file = new File(str);
				ok &= file.exists();
				nextFile = false;
			}		    
			else if(str.compareTo("ip") == 0){
				nextIp = true;
			}
			else if(str.compareTo("port") == 0){
				nextPort = true;
			}
			else if(str.compareTo("random") == 0){
				nextRandom = true;
			}
			else if(str.compareTo("file") == 0){
				nextFile = true;
			}
			else{
				return false;
			}
		}
		return ok;	
	}

	/**
	 * Lanza el programa
	 * @param args Argumentos pasados por consola deben ser "ip port mode"
	 * 			ip -> Dirección ip del servidor
	 * 			port -> Puerto del servidor
	 * 			mode -> puede ser "random" para indicar mensajes aleatorios o la ruta del archivo con los mensajes seteados
	 */
	public static void main(String[] args){
		if(!parserCommandLineArgs(args)) return;
		
		//Mensajes al azar al servidor
		if(isRandom){
			for(int i = 0; i < cantusers; i++){
				try{
					Telex.Client client = new Telex.Client(i, null);
					clients.add(client);
					client.start();
					Thread.sleep(1000);
				}catch(Exception e){
					System.out.println("Error al crear el cliente " + i);
				}
			}
		}
		//Mensajes leidos desde un archivo
		else{
			parseFile();
			for(Integer i: Collections.list(table.keys())){
				try{
					Telex.Client client = new Telex.Client(i, table.get(i));
					clients.add(client);
					client.start();
					Thread.sleep(1000);
				}catch(Exception e){
					System.out.println("Error al crear el cliente " + i);
				}
			}
		}
		while(!clients.isEmpty()){
			try {
				Thread.sleep(2000);
			} catch (InterruptedException e) {}
		}
	}

}
